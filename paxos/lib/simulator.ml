[@@@ocaml.warning "-27-26"]

open Base
open Time
open Counter
open Event_bus
open Sim_event
open Event_scheduler
open Message
open Types
open Log

(**
  Implements the Runtime interface using a discrete-time event scheduler.
*)
module Simulator = struct
  module V = Value_string.Value_string
  module B = Event_bus
  module NodeImpl = Node.Make_node (V) (B)

  type msg = V.t Message.t

  type event = Sim_event.t

  type int_set = (int, Base.Int.comparator_witness) Base.Set.t

  type partition_id = int

  type partition =
    { id: partition_id
    ; mutable member_node_ids: int_set
    ; bus: V.t Message.t B.t
    ; mutable cluster_size: int }

  type registries =
    { partition_registry: (partition_id, partition) Hashtbl.t
    ; node_registry: (Types.node_id, NodeImpl.t) Hashtbl.t
    ; node_alias_registry: (string, NodeImpl.t) Hashtbl.t
    ; node_to_partition: (Types.node_id, partition_id) Hashtbl.t }

  type counters =
    { event_id_counter: Counter.t
    ; msg_id_counter: Counter.t
    ; node_id_counter: Counter.t
    ; partition_id_counter: Counter.t }

  type settings =
    {max_ticks: int option; deterministic_seed: int option; log_jsonl: bool}

  type t =
    { mutable halted: bool
    ; mutable clock: Time.clock
    ; mutable registries: registries
    ; logger: Logger.t
    ; scheduler: Event_scheduler.t ref
    ; event_callbacks: (Sim_event.t -> unit) list ref
    ; counters: counters
    ; settings: settings }

  let next_event_id ({counters= {event_id_counter; _}; _} : t) =
    event_id_counter |> Counter.next

  let next_msg_id ({counters= {msg_id_counter; _}; _} : t) =
    msg_id_counter |> Counter.next

  let next_node_id ({counters= {node_id_counter; _}; _} : t) =
    node_id_counter |> Counter.next

  let current_time ({clock; _} : t) = clock |> Time.now

  (******************************************)
  (* Simulation core loop                   *)
  (******************************************)

  let dispatch_heartbeat ({registries= {partition_registry; _}; clock; _} as sim)
      =
    let msg =
      Message.make_heartbeat_msg ~msg_id:(next_msg_id sim)
        ~time:(Time.now clock)
      |> Message.Time
    in
    partition_registry |> Hashtbl.data
    |> List.iter ~f:(fun ({bus; _} : partition) ->
           msg |> Event_bus.publish_broadcast bus ~topic:Types.Time )

  let drain_buses {registries= {partition_registry; _}; _} =
    partition_registry |> Hashtbl.data
    |> List.iter ~f:(fun p -> p.bus |> Event_bus.drain)

  let step sim =
    let now = current_time sim in
    let due_events = Event_scheduler.pop_due_events !(sim.scheduler) now in
    List.iter due_events ~f:(fun ev ->
        ev.action () ;
        List.iter !(sim.event_callbacks) ~f:(fun cb -> cb ev) ) ;
    sim |> drain_buses ;
    Time.tick sim.clock ;
    sim |> dispatch_heartbeat ;
    Logger.tick sim.logger
      ~timestamp:(sim.clock |> Time.now |> Int.to_string_hum)
      ~msg:"...sim paused before this tick starts" ()

  let start sim =
    sim.halted <- false ;
    while not sim.halted do
      step sim
    done

  let stop sim = sim.halted <- true

  let pause = stop

  let reset sim =
    sim.halted <- false ;
    sim.clock <- Time.create_clock () ;
    sim.scheduler := Event_scheduler.create () ;
    sim.registries.node_registry |> Hashtbl.clear ;
    sim.registries.partition_registry |> Hashtbl.clear ;
    sim.registries.node_to_partition |> Hashtbl.clear ;
    sim.event_callbacks := []

  (******************************************)
  (* Node and Partition management          *)
  (******************************************)

  let payload_serialiser = Message.payload_serialiser_of V.sexp_of_t

  let create_partition ?(cluster_size = 0) id =
    let bus = B.create ~payload_serialiser () in
    {id; member_node_ids= Set.empty (module Int); bus; cluster_size}

  let add_node_to_partition_exn
      ({registries= {partition_registry; node_to_partition; _}; _} : t)
      ?(partition_id = 1) (node : NodeImpl.t) =
    let ({member_node_ids; bus; _} as partition) : partition =
      Hashtbl.find_exn partition_registry partition_id
    in
    let node_id = node |> NodeImpl.id_of in
    partition.member_node_ids <- Base.Set.add member_node_ids node_id ;
    Hashtbl.add_exn node_to_partition ~key:node_id ~data:partition_id ;
    node |> NodeImpl.register_node_with_bus bus

  let remove_node_from_partition_exn
      ({registries= {partition_registry; node_to_partition; _}; _} : t)
      (node : NodeImpl.t) =
    let node_id = node |> NodeImpl.id_of in
    match node_id |> Hashtbl.find_and_remove node_to_partition with
    | None ->
        Stdio.print_endline "WALDO: this shouldn't be happening..." ;
        ()
    | Some partition_id ->
        let ({member_node_ids; bus; _} as partition) : partition =
          partition_id |> Hashtbl.find_exn partition_registry
        in
        node |> NodeImpl.deregister_node_from_bus bus |> ignore ;
        partition.member_node_ids <- Base.Set.remove member_node_ids node_id

  let get_partition_for_node_exn
      ({registries= {partition_registry; node_to_partition; _}; _} : t)
      (node_id : Types.node_id) : partition =
    Hashtbl.find_exn node_to_partition node_id
    |> Hashtbl.find_exn partition_registry

  let move_node_to_partition_exn
      ( { registries= {partition_registry; node_registry; _}
        ; counters= {partition_id_counter; _}
        ; _ } as sim ) ~(dest : partition_id) node_id =
    let node = node_id |> Hashtbl.find_exn node_registry in
    node |> remove_node_from_partition_exn sim ;
    let {id= partition_id; _} =
      dest
      |> Hashtbl.find_or_add partition_registry ~default:(fun () ->
             partition_id_counter |> Counter.next
             |> create_partition ~cluster_size:1 )
    in
    node |> add_node_to_partition_exn sim ~partition_id

  let get_node_by_alias {registries= {node_alias_registry; _}; _} alias =
    alias |> Hashtbl.find node_alias_registry

  let make_node_change_partitions
      ({registries= {node_alias_registry; _}; _} as sim) ~args alias =
    let dest = args |> Option.value_exn |> List.hd_exn |> Int.of_string in
    Stdio.printf
      "[make_node_change_partition] %s to be shifted to partition dest \
       partition={%d}\n"
      alias dest ;
    alias
    |> Hashtbl.find_exn node_alias_registry
    |> NodeImpl.id_of
    |> move_node_to_partition_exn sim ~dest
    |> ignore

  let make_node_idle sim ~time alias =
    match alias |> get_node_by_alias sim with
    | None ->
        Stdio.printf
          "WARNING: Couldn't find any node with alias=(%s); can't make that \
           idle!\n\
           %!"
          alias
    | Some node ->
        let node_id = node |> NodeImpl.id_of in
        let msg =
          Message.make_sim_control_idle_node ~msg_id:(sim |> next_msg_id) ~time
            ~node_id
          |> Message.Control
        in
        let thunk = ((Types.Simulation_control, Some node_id), msg) in
        let {bus; _} = node_id |> get_partition_for_node_exn sim in
        thunk |> B.enqueue bus

  let make_node_inactive sim ~time alias =
    match alias |> get_node_by_alias sim with
    | None ->
        Stdio.printf
          "WARNING: Couldn't find any node with alias=(%s); can't make that \
           inactive!\n\
           %!"
          alias
    | Some node ->
        let node_id = node |> NodeImpl.id_of in
        let msg_id = sim |> next_msg_id in
        let msg =
          Message.make_sim_control_inactive_node ~msg_id ~time ~node_id
          |> Message.Control
        in
        let thunk = ((Types.Simulation_control, Some node_id), msg) in
        let {bus; _} = node_id |> get_partition_for_node_exn sim in
        thunk |> B.enqueue bus

  let make_node_active sim ~time alias =
    match alias |> get_node_by_alias sim with
    | None ->
        Stdio.printf
          "WARNING: Couldn't find any node with alias=(%s); can't make that \
           active!\n\
           %!"
          alias
    | Some node ->
        let node_id = node |> NodeImpl.id_of in
        let msg_id = sim |> next_msg_id in
        let msg =
          Message.make_sim_control_activate_node ~msg_id ~time ~node_id
          |> Message.Control
        in
        let thunk = ((Types.Simulation_control, Some node_id), msg) in
        let {bus; _} = node_id |> get_partition_for_node_exn sim in
        thunk |> B.enqueue bus

  (******************************************)
  (* Debugging / diagnostics                *)
  (******************************************)

  let print_bus_stats ({registries= {partition_registry; _}; _} : t) =
    partition_registry |> Hashtbl.data
    |> List.iter ~f:(fun p -> B.print_stats p.bus)

  (********************************************)
  (* Hydration of specs into runtime entities *)
  (********************************************)

  let hydrate_node ({counters= {node_id_counter; _}; _} : t)
      (node_spec : NodeImpl.spec) =
    {node_spec with node_id= node_id_counter |> Counter.next}
    |> NodeImpl.of_spec

  let seed_node_exn
      ({ registries= {partition_registry; node_registry; node_alias_registry; _}
       ; _ } as sim :
        t ) (node : NodeImpl.t) =
    let default_partition = Hashtbl.find_exn partition_registry 1 in
    Hashtbl.add_exn node_registry ~key:(node |> NodeImpl.id_of) ~data:node ;
    Hashtbl.add_exn node_alias_registry
      ~key:(node |> NodeImpl.alias_of)
      ~data:node ;
    node |> add_node_to_partition_exn sim |> ignore

  (** convenience routine for converting string to V.t *)
  let make_val s = V.t_of_sexp (Sexplib.Sexp.Atom s)

  let hydrate_proposal_event
      ({ registries= {partition_registry; node_registry; node_alias_registry; _}
       ; _ } as sim :
        t ) ({id; target; data; time; args; _} : Sim_event.spec) =
    match (target, data) with
    | Some alias, Some value_str -> (
      match get_node_by_alias sim alias with
      | Some captured_node ->
          (* TODO: seq number needs to be data-injectable *)
          let action () =
            let node =
              captured_node |> NodeImpl.id_of |> Hashtbl.find_exn node_registry
            in
            let seq =
              Option.value_map args
                ~f:(fun l -> l |> List.hd_exn |> Int.of_string)
                ~default:1
            in
            let assertion : V.t Types.paxos_assertion_state =
              { Types.proposal=
                  Types.make_proposal_id ~node:(node |> NodeImpl.id_of) ~seq
              ; value= make_val value_str }
            in
            let msg_id = sim |> next_msg_id in
            let {bus; _} =
              node |> NodeImpl.id_of |> get_partition_for_node_exn sim
            in
            NodeImpl.propose node ~msg_id ~time ~bus ~assertion
          in
          { Sim_event.id= Option.value id ~default:(next_event_id sim)
          ; time
          ; args
          ; kind= Sim_event.Custom "proposal"
          ; action }
      | None ->
          failwith ("Unknown node alias: " ^ alias) )
    | _ ->
        failwith "Malformed proposal event spec"

  let hydrate_metric_event sim ({data; id; time; args; _} : Sim_event.spec) =
    match data with
    | Some "print_bus_stats" ->
        { Sim_event.id= Option.value id ~default:(next_event_id sim)
        ; time
        ; args
        ; kind= Sim_event.Metric
        ; action= (fun () -> print_bus_stats sim) }
    | _ ->
        failwith ("Unknown metric data: " ^ Option.value ~default:"" data)

  let hydrate_control_event sim
      ({target; data; id; time; args; _} : Sim_event.spec) =
    let action =
      match (target, data) with
      | Some alias, Some "deactivate" | Some alias, Some "inactivate" ->
          fun () -> alias |> make_node_inactive sim ~time
      | Some alias, Some "make_idle" ->
          fun () -> alias |> make_node_idle sim ~time
      | Some alias, Some "activate" | Some alias, Some "reactivate" ->
          fun () -> alias |> make_node_active sim ~time
      | Some alias, Some "partition" ->
          fun () -> alias |> make_node_change_partitions sim ~args
      | _ ->
          failwith "Malformed control event spec"
    in
    { Sim_event.id= Option.value id ~default:(next_event_id sim)
    ; args
    ; time
    ; kind= Sim_event.Control
    ; action }

  let hydrate_fallback sim (spec : Sim_event.spec) =
    let other = spec.kind in
    { Sim_event.id= Option.value spec.id ~default:(next_event_id sim)
    ; args= spec.args
    ; time= spec.time
    ; kind= Sim_event.Custom other
    ; action=
        (* TODO: fix the sim control series of steps *)
        (fun () -> Stdio.printf "[Sim_event] Unhandled kind %s\n%!" other ) }

  let hydrate_event sim (spec : Sim_event.spec) =
    match spec.kind with
    | "proposal" ->
        hydrate_proposal_event sim spec
    | "metric" ->
        hydrate_metric_event sim spec
    | "control" ->
        hydrate_control_event sim spec
    | _other ->
        hydrate_fallback sim spec

  (******************************************)
  (* Event API                              *)
  (******************************************)

  let seed_event sim ev = Event_scheduler.add_event !(sim.scheduler) ev

  let seed_events sim evs = List.iter evs ~f:(fun ev -> ev |> seed_event sim)

  let inline_event sim ~time ~kind ~action ~args =
    let id = next_event_id sim in
    let ev = {Sim_event.id; time; kind; action; args} in
    seed_event sim ev

  let on_event sim callback =
    sim.event_callbacks := callback :: !(sim.event_callbacks)

  (******************************************)
  (* Construction / configuration           *)
  (******************************************)

  type spec =
    { max_ticks: int option [@default None] [@yojson_drop_default]
    ; deterministic_seed: int option [@default None] [@yojson_drop_default]
    ; log_jsonl: bool }
  [@@deriving sexp, yojson]

  let of_spec {max_ticks; deterministic_seed; log_jsonl} : t =
    let partition_registry = Hashtbl.create (module Int) in
    (* INVARIANT: a node will ALWAYS be a member of a particular partition. *)
    let initial_partition = create_partition 1 in
    Hashtbl.set partition_registry ~key:initial_partition.id
      ~data:initial_partition ;
    { halted= false
    ; clock= Time.create_clock ()
    ; scheduler= ref (Event_scheduler.create ())
    ; event_callbacks= ref []
    ; logger= Logger.create Stdlib.__MODULE__ ()
    ; registries=
        { partition_registry
        ; node_registry= Hashtbl.create (module Int)
        ; node_alias_registry= Hashtbl.create (module String)
        ; node_to_partition= Hashtbl.create (module Int) }
    ; counters=
        { event_id_counter= Counter.create 1
        ; partition_id_counter= Counter.create 2
        ; msg_id_counter= Counter.create 1
        ; node_id_counter= Counter.create 1 }
    ; settings= {max_ticks; deterministic_seed; log_jsonl} }
end

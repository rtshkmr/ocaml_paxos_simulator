(*
IMPROVEMENT CONSIDERATIONS:
===========================
1. Error-handling discipline:
   - hydration functions should have custom errors being thrown

   - we should guard against errors, but the current main source of error would be the input files. We shall make an assumption that input files are accurately defined and side-step the defensive code needed for this.

   - currently, I'm just calling all *_exn functions dangerously to make failures more visible.

2. use of `ignore` is likely a code smell here. the use of ignore suggests we're calling a function for its side effects but not utilising the result. If the result isn't necessary, we should consider adjusting the function signature to return unit instead of a value to make this more explicit.
*)
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
module Simulator : Runtime.S = struct
  module V = Value_string.Value_string
  module B = Event_bus
  module N = Node.Make_node (V) (B)

  type msg = V.t Message.t

  type event = Sim_event.t

  type int_set = (int, Base.Int.comparator_witness) Base.Set.t

  type partition_id = int

  type partition =
    {id: partition_id; mutable member_node_ids: int_set; bus: V.t Message.t B.t}

  type registries =
    { partition_registry: (partition_id, partition) Hashtbl.t
    ; node_registry: (Types.node_id, N.t) Hashtbl.t
    ; node_alias_registry: (string, N.t) Hashtbl.t
    ; node_to_partition: (Types.node_id, partition_id) Hashtbl.t }

  type counters =
    { event_id_counter: Counter.t
    ; msg_id_counter: Counter.t
    ; node_id_counter: Counter.t
    ; partition_id_counter: Counter.t }

  type settings = {max_ticks: int option}

  type t =
    { mutable halted: bool
    ; mutable clock: Time.clock
    ; registries: registries
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

  (* %%%%%%%%% Simulation Core Loop: %%%%%%%%%%% *)

  let dispatch_heartbeat ({registries= {partition_registry; _}; clock; _} as sim)
      =
    let msg =
      Message.make_heartbeat_msg ~msg_id:(sim |> next_msg_id)
        ~time:(Time.now clock)
    in
    partition_registry |> Hashtbl.data
    |> List.iter ~f:(fun ({bus; _} : partition) ->
           msg |> Event_bus.publish_broadcast bus ~topic:Types.Time )

  let drain_buses {registries= {partition_registry; _}; _} =
    partition_registry |> Hashtbl.data
    |> List.iter ~f:(fun {bus; _} -> bus |> Event_bus.drain)

  let step ({scheduler; event_callbacks; clock; logger; _} as sim) =
    let now = sim |> current_time in
    now
    |> Event_scheduler.pop_due_events !scheduler
    |> List.iter ~f:(fun ev ->
           ev.action () ;
           List.iter !event_callbacks ~f:(fun cb -> cb ev) ) ;
    sim |> drain_buses ;
    clock |> Time.tick ;
    sim |> dispatch_heartbeat ;
    Logger.tick logger ~timestamp:(now |> Int.to_string_hum) ~msg:"" ()

  let is_runnable ({settings= {max_ticks; _}; _} as sim) =
    match max_ticks with Some limit -> sim |> current_time < limit | _ -> true

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

  (* %%%%%%%%% Node, Partition management %%%%%%%%%%% *)
  let create_partition id =
    let bus = B.create ~payload_to_string:(V.sexp_of_t |> Message.to_string) in
    {id; member_node_ids= Set.empty (module Int); bus}

  let add_node_to_partition_exn
      ({registries= {partition_registry; node_to_partition; _}; _} : t)
      ?(partition_id = 1) (node : N.t) =
    let ({member_node_ids; bus; _} as partition) : partition =
      Hashtbl.find_exn partition_registry partition_id
    in
    let node_id = node |> N.id_of in
    partition.member_node_ids <- Base.Set.add member_node_ids node_id ;
    Hashtbl.add_exn node_to_partition ~key:node_id ~data:partition_id ;
    node |> N.register_node_with_bus bus

  let remove_node_from_partition_exn
      ({registries= {partition_registry; node_to_partition; _}; _} : t)
      (node : N.t) =
    let node_id = node |> N.id_of in
    match node_id |> Hashtbl.find_and_remove node_to_partition with
    | None ->
        (* TODO: figure out error handling? *)
        Stdio.print_endline "WALDO: this shouldn't be happening..." ;
        ()
    | Some partition_id ->
        let ({member_node_ids; bus; _} as partition) : partition =
          partition_id |> Hashtbl.find_exn partition_registry
        in
        node |> N.deregister_node_from_bus bus |> ignore ;
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
             partition_id_counter |> Counter.next |> create_partition )
    in
    node |> add_node_to_partition_exn sim ~partition_id

  let enqueue_thunk sim thunk =
    let alias = "~narrator" in
    let (_, node_id_opt), _ = thunk in
    let {bus; _} =
      node_id_opt |> Option.value_exn |> get_partition_for_node_exn sim
    in
    B.enqueue bus ~alias thunk

  let get_nodes {registries= {node_registry; _}; _} =
    node_registry |> Hashtbl.data

  let get_node_by_alias {registries= {node_alias_registry; _}; _} alias =
    alias |> Hashtbl.find node_alias_registry

  let make_node_change_partitions
      ({registries= {node_alias_registry; _}; _} as sim) ~args alias =
    let dest = args |> Option.value_exn |> List.hd_exn |> Int.of_string in
    (* TODO [LOG] shift to logger *)
    Stdio.printf
      "[make_node_change_partition] %s to be shifted to partition dest \
       partition={%d}\n"
      alias dest ;
    alias
    |> Hashtbl.find_exn node_alias_registry
    |> N.id_of
    |> move_node_to_partition_exn sim ~dest
    |> ignore

  let control_node_state_change sim ~time alias action_type =
    let node_id =
      alias |> get_node_by_alias sim |> Option.value_exn |> N.id_of
    in
    let factory =
      match action_type with
      | `Idle ->
          Message.make_sim_control_idle_node
      | `Inactive ->
          Message.make_sim_control_inactive_node
      | `Active ->
          Message.make_sim_control_activate_node
    in
    let msg = factory ~msg_id:(sim |> next_msg_id) ~time ~node_id in
    ((Types.Simulation_control, Some node_id), msg) |> enqueue_thunk sim

  let make_node_inactive sim ~time alias =
    `Inactive |> control_node_state_change sim ~time alias

  let make_node_active sim ~time alias =
    `Active |> control_node_state_change sim ~time alias

  let make_node_idle sim ~time alias =
    `Idle |> control_node_state_change sim ~time alias

  (* %%%%%%%%% Debugging & Diagnostics %%%%%%%%%%% *)

  let print_bus_stats ({registries= {partition_registry; _}; _} : t) =
    partition_registry |> Hashtbl.data
    |> List.iter ~f:(fun p -> B.print_stats p.bus)

  (* %%%%%%%%% Hydration: Static Spec to Runtime Struct %%%%%%%%%% *)

  let hydrate_node sim spec =
    {spec with node_id= sim |> next_node_id} |> N.of_spec

  let seed_node_exn
      ({ registries= {partition_registry; node_registry; node_alias_registry; _}
       ; _ } as sim :
        t ) (node : N.t) =
    let {id= partition_id; _} = Hashtbl.find_exn partition_registry 1 in
    Hashtbl.add_exn node_registry ~key:(node |> N.id_of) ~data:node ;
    Hashtbl.add_exn node_alias_registry ~key:(node |> N.alias_of) ~data:node ;
    node |> add_node_to_partition_exn sim ~partition_id |> ignore

  let seed_node_from_spec sim spec =
    spec |> hydrate_node sim |> seed_node_exn sim

  let seed_nodes_from_specs sim specs =
    specs |> List.iter ~f:(seed_node_from_spec sim)

  (** convenience routine for converting string to V.t *)
  let make_val s = V.t_of_sexp (Sexplib.Sexp.Atom s)

  let hydrate_proposal_event sim
      ({id; target; data; time; args; _} : Sim_event.spec) =
    let parse_seq args =
      args
      |> Option.value_map
           ~f:(fun l -> l |> List.hd_exn |> Int.of_string)
           ~default:1
    in
    match (target, data) with
    | None, _ | _, None ->
        failwith "Malformed proposal event spec"
    | Some alias, Some value_str ->
        { Sim_event.id= Option.value id ~default:(sim |> next_event_id)
        ; time
        ; args
        ; kind= "proposal" |> Sim_event.Custom
        ; action=
            (fun () ->
              let node = alias |> get_node_by_alias sim |> Option.value_exn in
              let {bus; _} = node |> N.id_of |> get_partition_for_node_exn sim
              and msg_id = sim |> next_msg_id
              and proposal =
                Types.make_proposal_id ~node:(node |> N.id_of)
                  ~seq:(args |> parse_seq)
              in
              node
              |> N.propose ~msg_id ~time ~bus
                   ~assertion:{proposal; value= value_str |> make_val} ) }

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

  let control_event_hydrators =
    [ ("deactivate", make_node_inactive)
    ; ("inactivate", make_node_inactive)
    ; ("make_idle", make_node_idle)
    ; ("activate", make_node_active)
    ; ("reactivate", make_node_active) ]
    |> Hashtbl.of_alist_exn (module String) ~growth_allowed:false

  let make_control_action sim ({target; data; time; args; _} : Sim_event.spec) =
    match (target, data) with
    | Some alias, Some "partition" ->
        fun () -> alias |> make_node_change_partitions sim ~args
    | Some alias, Some cmd ->
        fun () ->
          alias |> (cmd |> Hashtbl.find_exn control_event_hydrators) sim ~time
    | _ ->
        failwith "Malformed control event spec"

  let hydrate_control_event sim ({id; time; args; _} as spec : Sim_event.spec) =
    { Sim_event.id= id |> Option.value ~default:(next_event_id sim)
    ; args
    ; time
    ; kind= Sim_event.Control
    ; action= spec |> make_control_action sim }

  let hydrate_fallback sim (spec : Sim_event.spec) =
    let other = spec.kind in
    { Sim_event.id= Option.value spec.id ~default:(next_event_id sim)
    ; args= spec.args
    ; time= spec.time
    ; kind= Sim_event.Custom other
    ; action=
        (* TODO: fix the sim control series of steps *)
        (* TODO [LOG] shift to logger *)
        (fun () -> Stdio.printf "[Sim_event] Unhandled kind %s\n%!" other ) }

  let event_hydrators =
    [ ("proposal", hydrate_proposal_event)
    ; ("metric", hydrate_metric_event)
    ; ("control", hydrate_control_event) ]
    |> Hashtbl.of_alist_exn (module String) ~growth_allowed:false

  let hydrate_event sim ({kind; _} as spec : Sim_event.spec) =
    kind
    |> Hashtbl.find event_hydrators
    |> Option.value ~default:hydrate_fallback
    |> fun hydrator -> spec |> hydrator sim

  (* %%%%%%%%% Event API %%%%%%%%%%% *)

  let seed_event sim ev = Event_scheduler.add_event !(sim.scheduler) ev

  let seed_events sim evs = List.iter evs ~f:(fun ev -> ev |> seed_event sim)

  let seed_event_from_spec (sim : t) (spec : Sim_event.spec) =
    spec |> hydrate_event sim |> seed_event sim

  let seed_events_from_specs (sim : t) (specs : Sim_event.spec list) =
    specs |> List.iter ~f:(seed_event_from_spec sim)

  let on_event sim callback =
    sim.event_callbacks := callback :: !(sim.event_callbacks)

  (* %%%%%%%%% Construction and Configuration %%%%%%%%%%% *)

  type spec = {max_ticks: int option [@default None] [@yojson_drop_default]}
  [@@deriving sexp, yojson]

  let of_spec {max_ticks} : t =
    let partition_registry = Hashtbl.create (module Int) in
    (* NOTE: [INVARIANT] a node will ALWAYS be a member of a particular partition. *)
    let ({id; _} as initial_partition) = create_partition 1 in
    Hashtbl.set partition_registry ~key:id ~data:initial_partition ;
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
        ; partition_id_counter= Counter.create 2 (* we consume one above *)
        ; msg_id_counter= Counter.create 1
        ; node_id_counter= Counter.create 1 }
    ; settings= {max_ticks} }
end

(*
IMPROVEMENT CONSIDERATIONS:
===========================
1. Error-handling discipline:
   - hydration functions should have custom errors being thrown

   - we should guard against errors, but the current main source of error would be the input files. We shall make an assumption that input files are accurately defined and side-step the defensive code needed for this.

   - currently, I'm just calling all *_exn functions dangerously to make failures more visible.

2. use of `ignore` is likely a code smell here. the use of ignore suggests we're calling a function for its side effects but not utilising the result. If the result isn't necessary, we should consider adjusting the function signature to return unit instead of a value to make this more explicit.

3. there's a huge code smell in the form of using [failwith] because this will error out immediately. My intent was to just let it be so all errors bubble up and it's faster for me to rudimentarily check correctness, but seemsl like I should have just gone with Result struct wrapping from the beginning.
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
open Log_types

(**
  Discrete-time deterministic simulator for Paxos protocol testing.

  {b Simulation model:}
  - Logical time advances in discrete ticks (not wall-clock)
  - Event scheduler holds timed actions (node proposals, faults, metrics)
  - Each tick: pop due events → execute → drain buses → tick clock
  - Fully deterministic: same seed + scenario = same trace

  {b Partitioning:}
  Nodes belong to partitions (network islands). Each partition has its own bus.
  Nodes in different partitions cannot communicate (simulates network split).
  {[
    (* Move node 3 from partition 1 to partition 2 *)
    move_node_to_partition_exn sim ~dest:2 node_id:3
  ]}

  {b Control flow:}
  {v
    ┌────────────┐
    │  Scenario  │ (JSON config)
    └─────┬──────┘
          │ hydrate
          ▼
    ┌────────────┐      ┌──────────────┐
    │ Simulator  ├─────►│EventScheduler│
    │            │      └──────┬───────┘
    │  - Nodes   │             │ pop due events
    │  - Partns  │◄────────────┘
    │  - Clock   │
    └─────┬──────┘
          │ tick
          ▼ drain buses → deliver messages
  v}

  {b REPL commands:}
  - [space] = pause
  - [r] = resume
  - [q] = quit
  - [/inspect/sim/state] = dump partitions + nodes
  - [/inspect/node/state/<alias>] = dump FSM state

  See bin/simulation.ml for entry point.
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

  let find_bus ({registries= {partition_registry; _}; _} : t) bus_id =
    let partitions = Hashtbl.data partition_registry in
    List.find partitions ~f:(fun ({bus; _} : partition) ->
        phys_equal (Event_bus.id_of bus) bus_id )
    |> Option.map ~f:(fun partition -> partition.bus)

  let logger_of t = t.logger

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
    Logger.tick logger ~timestamp:(now |> Int.to_string_hum) ~msg:"" () ;
    now
    |> Event_scheduler.pop_due_events !scheduler
    |> List.iter ~f:(fun ev ->
        ev.action () ;
        List.iter !event_callbacks ~f:(fun cb -> cb ev) ) ;
    sim |> drain_buses ;
    clock |> Time.tick ;
    sim |> dispatch_heartbeat

  let is_runnable ({settings= {max_ticks; _}; _} as sim) =
    match max_ticks with
    | Some limit ->
        sim |> current_time <= limit
    | _ ->
        true

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
  let create_partition sim id =
    let bus =
      B.create
        ~log_level:(sim |> logger_of |> Logger.get_level)
        ~payload_to_string:(V.sexp_of_t |> Message.to_string)
        ()
    in
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
        let err_msg =
          Printf.sprintf
            "Node %d doesn't exist. Registry data is likely corrupted." node_id
        in
        failwith err_msg
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
          partition_id_counter |> Counter.next |> create_partition sim )
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
      ({logger; registries= {node_alias_registry; _}; _} as sim) ~args alias =
    let dest = args |> Option.value_exn |> List.hd_exn |> Int.of_string in
    let log_msg =
      Printf.sprintf
        "[make_node_change_partition] %s to be shifted to partition dest \
         partition={%d}\n"
        alias dest
    in
    Logger.subroutine_flow ~alias logger ~routine:Stdlib.__FUNCTION__
      ~msg:log_msg () ;
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

  let sync_node_logger sim node =
    let {level= global_log_level; _} : Logger.t = logger_of sim in
    let node_logger = N.logger_of node in
    Logger.set_level node_logger global_log_level ;
    node

  let seed_node_from_spec sim spec =
    spec |> hydrate_node sim |> sync_node_logger sim |> seed_node_exn sim

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

  let hydrate_metric_event ({registries= {partition_registry; _}; _} as sim)
      ({data; id; time; args; _} : Sim_event.spec) =
    match data with
    | Some "print_bus_stats" ->
        let action =
         fun () ->
          partition_registry |> Hashtbl.data
          |> List.iter ~f:(fun ({bus; _} : partition) ->
              Event_bus.display_stats bus )
        in
        { Sim_event.id= Option.value id ~default:(next_event_id sim)
        ; time
        ; args
        ; kind= Sim_event.Metric
        ; action }
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

  let hydrate_narration_event sim ({id; time; data; _} : Sim_event.spec) =
    let action =
     fun () ->
      let narration = Option.value data ~default:"" in
      let logger = logger_of sim in
      Logger.display_narration logger ~time ~narration
    in
    { Sim_event.id= id |> Option.value ~default:(next_event_id sim)
    ; args= None
    ; time
    ; kind= Sim_event.Narration
    ; action }

  let hydrate_fallback sim (spec : Sim_event.spec) =
    let other = spec.kind in
    { Sim_event.id= Option.value spec.id ~default:(next_event_id sim)
    ; args= spec.args
    ; time= spec.time
    ; kind= Sim_event.Custom other
    ; action=
        (fun () ->
          let msg = Printf.sprintf "[Sim_event] Unhandled kind %s\n%!" other in
          Logger.other sim.logger ~msg ) }

  let event_hydrators =
    [ ("proposal", hydrate_proposal_event)
    ; ("metric", hydrate_metric_event)
    ; ("narration", hydrate_narration_event)
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

  let seed_event_from_spec sim spec =
    spec |> hydrate_event sim |> seed_event sim

  let seed_events_from_specs sim specs =
    specs |> List.iter ~f:(seed_event_from_spec sim)

  let on_event sim callback =
    sim.event_callbacks := callback :: !(sim.event_callbacks)

  (* %%%%%%%%% Construction and Configuration %%%%%%%%%%% *)

  type spec =
    { max_ticks: int option [@default None] [@yojson_drop_default]
    ; log_level: Log_level.spec [@default "Info"] [@yojson_drop_default] }
  [@@deriving sexp, yojson]

  let of_spec ?(override_log_level = Log_level.Debug) {max_ticks; log_level} : t
      =
    let partition_registry = Hashtbl.create (module Int) in
    (* NOTE: [INVARIANT] a node will ALWAYS be a member of a particular partition. *)
    let level =
      log_level |> Log_level.of_spec |> Log_level.max override_log_level
    in
    let sim =
      { halted= false
      ; clock= Time.create_clock ()
      ; scheduler= ref (Event_scheduler.create ())
      ; event_callbacks= ref []
      ; logger= Logger.create ~level Stdlib.__MODULE__ ()
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
    in
    let ({id; _} as initial_partition) = create_partition sim 1 in
    Hashtbl.set partition_registry ~key:id ~data:initial_partition ;
    sim

  (* ==== dump helpers for introspection ========== *)
  let dump_partition_registry {registries= {partition_registry; _}; _} =
    let lines =
      partition_registry |> Hashtbl.to_alist
      |> List.map ~f:(fun (_partition_id, {id; member_node_ids; bus; _}) ->
          let member_count = Set.length member_node_ids in
          let bus_id = Event_bus.id_of bus in
          let member_ids =
            member_node_ids |> Set.to_list |> List.map ~f:Int.to_string
            |> String.concat ~sep:", "
          in
          Printf.sprintf "Partition %d: %d nodes [%s] communicating on bus %d"
            id member_count member_ids bus_id )
    in
    if List.is_empty lines then "No partitions"
    else String.concat ~sep:"\n" lines

  let dump_node_registry {registries= {node_registry; _}; _} =
    let lines =
      node_registry |> Hashtbl.to_alist
      |> List.map ~f:(fun (node_id, node) ->
          let alias = N.alias_of node in
          Printf.sprintf "Node %d (%s)" node_id alias )
    in
    if List.is_empty lines then "No nodes" else String.concat ~sep:"\n" lines

  let print_flush s =
    Stdio.print_endline s ;
    Out_channel.flush Stdio.stdout

  let parse_slash_cmd cmd =
    cmd |> String.strip
    |> String.chop_prefix_if_exists ~prefix:"/"
    |> String.split ~on:'/'
    |> List.filter ~f:(fun s -> not (String.is_empty s))

  let display_node_not_found alias =
    Printf.sprintf
      "There's no node with alias %s. Suggestion: try calling \
       /inspect/sim/state"
      alias
    |> print_flush

  let display_bus_not_found bus_id =
    Printf.sprintf
      "There's no bus with id %d. Suggestion: try calling /inspect/sim/state \
       to get the right bus id"
      bus_id
    |> print_flush

  let handle_slash_command sim cmd_s =
    match cmd_s |> parse_slash_cmd with
    | ["help"] ->
        Logger.slash_cmd_help sim.logger
    | ["inspect"; "sim"; "state"] ->
        let time = current_time sim in
        let partitions = dump_partition_registry sim in
        let nodes = dump_node_registry sim in
        let logger = sim.logger in
        Logger.inspect_sim_state logger ~time ~partitions ~nodes
    | ["inspect"; "node"; "state"; alias] -> (
      (* shortcut: only inspect by alias *)
      match get_node_by_alias sim alias with
      | None ->
          display_node_not_found alias
      | Some node ->
          let logger = N.logger_of node in
          let node_id, alias = (N.id_of node, N.alias_of node) in
          let dump = N.dump_state node in
          Logger.inspect_node_state logger ~node_id ~alias ~dump )
    | ["inspect"; "node"; "config"; alias] -> (
      match get_node_by_alias sim alias with
      | None ->
          display_node_not_found alias
      | Some node ->
          let logger = N.logger_of node in
          let node_id, alias = (N.id_of node, N.alias_of node) in
          let dump = N.dump_spec node in
          Logger.inspect_node_config logger ~node_id ~alias ~dump )
    | ["inspect"; "bus"; "stats"; bus_id] -> (
        let parsed_bus_id = Int.of_string_opt bus_id in
        if Option.is_none parsed_bus_id then
          Stdio.printf "The bus_id has to be an integer, you provided %s\n%!"
            bus_id
        else
          let parsed = Option.value_exn parsed_bus_id in
          match find_bus sim parsed with
          | None ->
              display_bus_not_found parsed
          | Some bus ->
              Event_bus.display_stats bus )
    | fallthrough ->
        Printf.sprintf "This command can't be handled, command parts:\n%s"
          (String.concat_lines fallthrough)
        |> print_flush
end

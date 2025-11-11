[@@@ocaml.warning "-27-33-69"]

open Base
open Time
open Counter
open Event_bus
open Node
open Runtime
open Sim_event
open Event_scheduler
open Message
open Types

(**
  Implements the Runtime interface using a discrete-time event scheduler.
*)
module Simulator = struct
  module V = Value_string.Value_string
  module B = Event_bus
  module S = Storage_mem.Storage_mem (V)
  module NodeImpl = Node.Make_node (V) (S) (B)

  type msg = V.t Message.t

  type event = Sim_event.t

  let payload_serialiser = Message.payload_serialiser_of V.sexp_of_t

  let bus = B.create ~payload_serialiser ()

  type t =
    { mutable halted: bool
    ; mutable clock: Time.clock
    ; scheduler: EventScheduler.t ref
    ; nodes: NodeImpl.t list ref
    ; event_callbacks: (Sim_event.t -> unit) list ref
    ; event_id_counter: Counter.t
    ; msg_id_counter: Counter.t
    ; max_ticks: int option
    ; deterministic_seed: int option
    ; log_jsonl: bool }

  let next_event_id t = Counter.next t.event_id_counter

  let next_msg_id t = Counter.next t.msg_id_counter

  let current_time sim = Time.now sim.clock

  (******************************************)
  (* Event API                              *)
  (******************************************)

  let seed_event sim ev = EventScheduler.add_event !(sim.scheduler) ev

  let seed_events sim evs = List.iter evs ~f:(fun ev -> ev |> seed_event sim)

  let seed_event_from_spec sim spec =
    let ev = Sim_event.of_spec spec in
    EventScheduler.add_event !(sim.scheduler) ev

  let seed_events_from_specs sim specs =
    List.iter specs ~f:(fun spec -> seed_event_from_spec sim spec)

  let inline_event sim ~time ~kind ~action =
    let id = next_event_id sim in
    let ev = {Sim_event.id; time; kind; action} in
    seed_event sim ev

  let on_event sim callback =
    sim.event_callbacks := callback :: !(sim.event_callbacks)

  (******************************************)
  (* Simulation core loop                   *)
  (******************************************)

  let dispatch_heartbeat sim =
    Message.make_heartbeat_msg ~msg_id:(next_msg_id sim)
      ~time:(Time.now sim.clock)
    |> Message.Time
    |> Event_bus.publish_broadcast bus ~topic:Types.Time

  let step sim =
    let now = current_time sim in
    let due_events = EventScheduler.pop_due_events !(sim.scheduler) now in
    List.iter due_events ~f:(fun ev ->
        ev.action () ;
        List.iter !(sim.event_callbacks) ~f:(fun cb -> cb ev) ) ;
    Event_bus.drain bus ;
    Time.tick sim.clock ;
    sim |> dispatch_heartbeat

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
    sim.scheduler := EventScheduler.create () ;
    sim.nodes := [] ;
    sim.event_callbacks := []

  (******************************************)
  (* Node management                        *)
  (******************************************)

  let add_node_to_sim sim (node_spec : NodeImpl.spec) =
    let next_id = 1 + List.length !(sim.nodes) in
    let overriden_spec = {node_spec with node_id= next_id} in
    let node =
      overriden_spec |> NodeImpl.of_spec |> NodeImpl.register_node_with_bus bus
    in
    sim.nodes := node :: !(sim.nodes) ;
    node

  let get_nodes sim = !(sim.nodes)

  (* Find a node by its integer id *)
  let get_node_by_id sim id =
    List.find !(sim.nodes) ~f:(fun (node : NodeImpl.t) ->
        node |> NodeImpl.id_of = id )

  (* Find a node by its alias string *)
  let get_node_by_alias sim alias =
    List.find !(sim.nodes) ~f:(fun (node : NodeImpl.t) ->
        String.equal (node |> NodeImpl.alias_of) alias )

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
        let msg_id = sim |> next_msg_id in
        let msg =
          Message.make_sim_control_idle_node ~msg_id ~time ~node_id
          |> Message.Control
        in
        let thunk = ((Types.Simulation_control, Some node_id), msg) in
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
        thunk |> B.enqueue bus

  (******************************************)
  (* Debugging / diagnostics                *)
  (******************************************)

  let print_bus_stats _ = B.print_stats bus

  (******************************************)
  (* Construction / configuration           *)
  (******************************************)

  type spec =
    {max_ticks: int option; deterministic_seed: int option; log_jsonl: bool}
  [@@deriving sexp, yojson]

  let of_spec {max_ticks; deterministic_seed; log_jsonl} : t =
    { halted= false
    ; clock= Time.create_clock ()
    ; scheduler= ref (EventScheduler.create ())
    ; nodes= ref []
    ; event_callbacks= ref []
    ; event_id_counter= Counter.create 1
    ; msg_id_counter= Counter.create 1
    ; max_ticks
    ; deterministic_seed
    ; log_jsonl }
end

[@@@ocaml.warning "-27-33-69"]

open Base
open Time
open Counter
open Event_bus
open Node
open Runtime
open Event_bus
open Event_scheduler
open Message
open Types

(**
  Implements the Runtime interface using a discrete-time event scheduler.
*)
module Simulator = struct
  (* concretizing modules *)
  module V = Value_string.Value_string
  module B = Event_bus
  module S = Storage_mem.Storage_mem (V)
  module NodeImpl = Node.Make_node (V) (S) (B)

  type msg = V.t Message.t (* shadow type *)

  let payload_serialiser = Message.payload_serialiser_of V.sexp_of_t

  type event = EventScheduler.event

  let bus = B.create ~payload_serialiser ()

  type t =
    { mutable halted: bool
    ; mutable clock: Time.clock
    ; scheduler: EventScheduler.t ref
    ; nodes: NodeImpl.t list ref
    ; event_callbacks: (event -> unit) list ref
    ; event_id_counter: Counter.t
    ; msg_id_counter: Counter.t
    ; max_ticks: int option
    ; deterministic_seed: int option
    ; log_jsonl: bool }

  let next_event_id t = Counter.next t.event_id_counter

  let next_msg_id t = Counter.next t.msg_id_counter

  (* TODO [REFACTOR] simulator is a little blown up right now and needs a cleanup. We can do this later. *)
  let make_event sim ?(id = next_event_id sim) ~time action () =
    EventScheduler.create_event id time action

  let enqueue_thunk sim ?(id = next_event_id sim) ~time
      (thunk : msg enqueuable_thunk) =
    let action = fun () -> Event_bus.enqueue bus thunk in
    make_event sim ~id ~time action ()

  let make_node_proposal_event sim ~(initiator : NodeImpl.t) ~assertion
      ~(time : Time.t) =
    let thunk () =
      let msg_id = next_msg_id sim in
      NodeImpl.propose initiator ~msg_id ~time ~bus ~assertion
    in
    make_event sim ~time thunk ()

  (** can be coordinated, can be controlled by simulator*)
  let add_node_to_sim sim (node_spec : NodeImpl.spec) =
    let overriding_node_id = 1 + List.length !(sim.nodes) in
    let overriden_spec = {node_spec with node_id= overriding_node_id} in
    let added_node =
      overriden_spec |> NodeImpl.of_spec |> NodeImpl.register_node_with_bus bus
    in
    sim.nodes := added_node :: !(sim.nodes) ;
    added_node

  (** This is not scheduled*)
  let broadcast_heartbeat sim ~msg_id =
    let time = Time.now sim.clock in
    let raw_msg = Message.make_heartbeat_msg ~msg_id ~time in
    let msg = Message.Time raw_msg in
    let topic = Types.Time in
    Event_bus.publish_broadcast bus ~topic msg

  let schedule_event sim event = EventScheduler.add_event !(sim.scheduler) event

  let on_event sim f = sim.event_callbacks := f :: !(sim.event_callbacks)

  let current_time sim = Time.now sim.clock

  (** This is one step that includes:
      1. simulator gathers all the events to be dispatched for this step and dispatches them
      2. we format a tick message for the current tick
      3. we drain the bus, letting the nodes react independently
      4. we move to the next tick and broadcast that via message-passing

      We should respect design principles such as:
      - our [Event_bus] will always be passive and reactive.
      - [Nodes] in the system will never be directly changed by the simulation, their internal state may only be updated via message passing.
      - time flows through the system via message-passing
  *)
  let step sim =
    let now = current_time sim in
    let sim_events_due = EventScheduler.pop_due_events !(sim.scheduler) now in
    List.iter ~f:(fun e -> e.action ()) sim_events_due ;
    Event_bus.drain bus ;
    Time.tick sim.clock ;
    broadcast_heartbeat sim ~msg_id:(next_msg_id sim)

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

  let get_nodes sim = !(sim.nodes)

  let print_bus_stats t = B.print_stats bus

  type spec =
    {max_ticks: int option; deterministic_seed: int option; log_jsonl: bool}

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

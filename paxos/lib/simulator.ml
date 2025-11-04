[@@@ocaml.warning "-27-33"] (** TODO: remove unused variable warnings*)
open Base
open Time
open Counter
open Event_bus
open Node
open Runtime
open Event_bus
open Event_scheduler
open Message

(*
  Implements the Runtime interface using a discrete-time event scheduler.
*)
module Simulator : Runtime = struct
  (* concretizing modules *)
  module V = Value_string.Value_string
  module B = Event_bus
  module S = Storage_mem.Storage_mem (V)
  module NodeImpl = Node.Make_node (V) (S) (B)

  type msg = V.t Message.t (* shadow type *)
  let msg_of_message (m: V.t Message.t): msg = m (* converts structurally equal type to shadow type *)

  type node = NodeImpl.t

  let id_of_node node = NodeImpl.id node


  type event = EventScheduler.event

  (* let bus = *)
  (*   B.create *)
  (*     ~logger:(fun topic msg -> *)
  (*       Printf.sprintf "[LOG][%s] %s" *)
  (*         (Sexp.to_string (Types.Types.sexp_of_topic topic)) *)
  (*         (Sexplib.Sexp.to_string (Message.sexp_of_t V.sexp_of_t msg)) ) *)
  (*     () *)

let bus =
  B.create
    ~logger:(fun topic msg ->
      let open Sexplib.Sexp in
      let topic_sexp = Types.Types.sexp_of_topic topic in
      let msg_sexp = Message.sexp_of_t V.sexp_of_t msg in
      let formatted =
        Sexp.to_string_hum (List [List [Atom "LOG"; topic_sexp]; msg_sexp])
      in
      formatted
    )
    ()

  type t =
    { mutable halted: bool
    ; mutable clock: Time.clock
    ; scheduler: EventScheduler.t ref
    ; nodes: node list ref
    ; event_callbacks: (event -> unit) list ref
    ; event_id_counter: Counter.t
    ; msg_id_counter: Counter.t
    }

  let next_event_id t = Counter.next t.event_id_counter
  let next_msg_id t = Counter.next t.msg_id_counter

  let make_event sim ?(id=next_event_id sim) ~time action () = EventScheduler.create_event id time action

  (* DEPRECATED *)
  let make_message_event sim ?(id=next_msg_id sim)  ~time ?to_node ~topic ~from:node ~msg () =
    let action () =
      (match to_node with
      | None -> Event_bus.publish_broadcast bus ~topic msg
      | Some target_node -> Event_bus.publish_unicast ~node_id:(NodeImpl.id target_node) bus ~topic msg)
    in
    let event = {id; EventScheduler.time = time; action} in
    event

  type msg_factory =  msg_id:int -> from:node -> ?to_node:node -> time:Time.t -> unit -> msg

  let create_message_event
      (sim : t)
      ~(topic : Types.Types.topic)
      ~(from : node)
      ?to_node
      ?(time : Time.t = Time.now sim.clock)
      ~(msg_factory : msg_factory)
      ()
    : event =
    let msg_id = next_msg_id sim in
    let msg = match to_node with
      | Some target_node ->  msg_factory ~msg_id ~from ~to_node:target_node ~time ()
      | None -> msg_factory ~msg_id ~from ~time ()
                in
    let to_node_id = match to_node with
      | None -> None
      | Some node -> Some (NodeImpl.id node) in
    let thunk = ((topic, to_node_id), msg) in
    let action () = Event_bus.enqueue bus thunk  in
    make_event sim ~time action ()

  let create ~config:_ =
    { halted= false
    ; clock= Time.create_clock ()
    ; scheduler= ref (EventScheduler.create ())
    ; nodes= ref []
    ; event_callbacks= ref []
    ; event_id_counter = Counter.create 1
    ; msg_id_counter = Counter.create 1
    }

  (** can be coordinated, can be controlled by simulator*)
  let base_state = NodeImpl.State.Echo
  let add_node_to_sim sim ~node_config =
    let new_node_id = 1 + List.length !(sim.nodes) in
    let new_node =
      NodeImpl.create ~bus ~config:node_config ~id:new_node_id ~state:base_state ()
    in
    sim.nodes := new_node :: !(sim.nodes) ;
    new_node

  let create_node_config_from_sim_spec (node_spec : Config.node_spec) :
      NodeImpl.config =
    let roles = List.map node_spec.roles ~f:NodeImpl.role_of_string in
    let initial_quorum = node_spec.initial_quorum in
    let storage = S.create () in
    let simulation = {NodeImpl.quorum= ref initial_quorum} in
    let topics = node_spec.topics in
    {NodeImpl.simulation; roles; storage; topics}

  let add_node sim ~node_spec =
    let node_config = create_node_config_from_sim_spec node_spec in
    add_node_to_sim sim ~node_config

  (** This is not scheduled*)
  let broadcast_heartbeat sim ~msg_id =
    let time = Time.now sim.clock in
    let raw_msg = Message.make_heartbeat_msg ~msg_id ~time in
    let msg = Message.Time raw_msg in
    let topic = Types.Types.Time in
    Event_bus.publish_broadcast bus ~topic msg
    (* let cb = fun () -> Event_bus.publish bus ~topic msg in *)
    (* let event_id = 2 in *)
    (* (\* FIXME: the id here needs a counter and everything -- this should be event_id*\) *)
    (* let event = {id=event_id; EventScheduler.time= Time.now sim.clock; action= (fun () -> cb ())} in *)
    (* Event_bus.publish   *)
    (* (\* EventScheduler    EventScheduler.add_event !(sim.scheduler) event *\) *)

  let schedule_event sim event =
    EventScheduler.add_event !(sim.scheduler) event

  let on_event sim f = sim.event_callbacks := f :: !(sim.event_callbacks)

  let current_time sim = Time.now sim.clock

  (** Simulator specific tick logic grouped as one.
      1. advance the logical clock
      2. send the heartbeat message
   *)

  let tick t =
    let msg = "..." in
    let formatted_tick_msg = Time.format_tick_msg t.clock ~msg () in
    Stdio.print_endline  formatted_tick_msg;
    Event_bus.drain bus;
    Time.tick t.clock;
    broadcast_heartbeat t ~msg_id:(next_msg_id t)

  (** This is one step that includes:
     1. simulator gathers all the events to be dispatched for this step
     3. the simulator clock will tick and the tick will propagate to all nodes

    We should respect design principles such as:
    - our [Event_bus] will always be passive and reactive.
    - [Nodes] in the system will never be directly changed by the simulation, their internal state may only be updated via message passing.
*)
  let step sim =
    let now = current_time sim in
    let sim_events_due = EventScheduler.pop_due_events !(sim.scheduler) now in
    (* Run all due events *)
    List.iter
      ~f:(fun ev ->
        ev.action ())
        (* List.iter ~f:(fun cb -> cb ev) !(sim.event_callbacks) ) *)
      sim_events_due ;

    tick sim

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

  let print_bus_stats t =
    B.print_stats bus;

end

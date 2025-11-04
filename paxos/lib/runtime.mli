open Time

(**
  Defines the abstract interface for a simulation runtime.
  A Runtime manages time, nodes, and event scheduling.
*)
module type Runtime = sig
  type t

  (* here, we expose the modules that will be used to create a node *)
  module V : Value.S

  module B : Event_bus.S

  module S : Storage.S

  type msg

  val msg_of_message : V.t Message.Message.t -> msg
  (** [msg_of_message] returns a Runtime.msg nominal type.

    FIXME SMELL: this is a hack, possible code smell because we have structural type equality but the nominal types are different.

    We can observe this in the simulation setup where our [send_message] expects the message to be [type Simulator.msg], but using the
    Constructor for [Message.Coordination] gives us type [V.t Message.Message.t].

    Seems like some sort of type definition drift. This feels like a smell in the design of things.
   *)

  type node

  type event

  val create : config:Config.t -> t
  (** Create a new (simulation) runtime from configuration. *)

  val add_node : t -> node_spec:Config.node_spec -> node
  (** Add a new node to the simulation. Returns the created node. *)

  val start : t -> unit
  (** Start continuous simulation until stopped. *)

  val stop : t -> unit
  (** Stop/pause the simulation loop. *)

  val step : t -> unit
  (** Execute one simulation tick (advance time, run due events). *)

  val get_nodes : t -> node list
  (** Get the list of registered nodes. *)

  val make_event : int -> int -> (unit -> unit) -> unit -> event
  (** Creates an general event that can be scheduled as a simulation event*)

  val make_message_event :
       int
    -> int
    -> ?to_node:node
    -> topic:Types.Types.topic
    -> from:node
    -> msg:msg
    -> unit
    -> event
  (** Creates a message passing event between nodes over a particular topic. Optionally specify destination to have a direct message. *)

  val schedule_event : t -> event -> unit

  val on_event : t -> (event -> unit) -> unit
  (** Subscribe to simulation-level events (for logging, metrics, etc.). *)

  val current_time : t -> Time.t
  (** Get current logical time. *)

  val pause : t -> unit
  (** Pause the simulation. Alias for [stop]. *)

  val reset : t -> unit
  (** Reset simulation to initial time and state. *)

  val print_bus_stats : t -> unit
  (** Gives a rudimentary print-dump of the state within the event bus used for the simulation.*)
end

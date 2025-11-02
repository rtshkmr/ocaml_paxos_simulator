open Time

(**
  Defines the abstract interface for a simulation runtime.
  A Runtime manages time, nodes, and event scheduling.
*)
module type Runtime = sig
  module V : Value.S

  module B : Event_bus.S

  module S : Storage.S

  type t

  type msg

  val msg_of_message : V.t Message.Message.t -> msg

  type node

  type event

  val create : config:Config.t -> t
  (** Create a new simulation runtime from configuration. *)

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

  val send_message :
       t
    -> topic:Types.Types.topic
    -> from:node
    -> to_:node option
    -> msg:msg
    -> unit
  (** Send a message between nodes over a particular topic. Optionally specify destination. *)

  val on_event : t -> (event -> unit) -> unit
  (** Subscribe to simulation-level events (for logging, metrics, etc.). *)

  val current_time : t -> Time.t
  (** Get current logical time. *)

  val pause : t -> unit
  (** Pause the simulation. Alias for [stop]. *)

  val reset : t -> unit
  (** Reset simulation to initial time and state. *)

  val print_bus_stats : t -> unit
end

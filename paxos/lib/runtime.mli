open Time
open Types

(**
  Defines the abstract interface for a simulation runtime.
  A Runtime manages time, nodes, and event scheduling.
*)
module type Runtime = sig
  type t

  include Has_spec with type t := t

  module V : Value.S

  module N : Node.S

  type msg = V.t Message.Message.t

  type event

  val start : t -> unit
  (** Start continuous simulation until stopped. *)

  val stop : t -> unit
  (** Stop/pause the simulation loop. *)

  val step : t -> unit
  (** Execute one simulation tick (advance time, run due events). *)

  val get_nodes : t -> N.t list
  (** Get the list of registered nodes. *)

  val make_event : t -> ?id:int -> time:int -> (unit -> unit) -> unit -> event
  (** Creates an general event that can be scheduled as a simulation event*)

  val enqueue_thunk :
       t
    -> ?id:int
    -> time:int
    -> msg Event_bus.Event_bus.enqueuable_thunk
    -> event

  val next_msg_id : t -> int

  val next_event_id : t -> int

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

  val make_node_proposal_event :
       t
    -> initiator:N.t
    -> assertion:V.t Types.paxos_assertion_state
    -> time:int
    -> event

  type spec =
    {max_ticks: int option; deterministic_seed: int option; log_jsonl: bool}

  val of_spec : spec -> t
end

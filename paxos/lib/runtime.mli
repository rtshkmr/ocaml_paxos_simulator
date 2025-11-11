open Time
open Types
open Sim_event

(**
  Defines the abstract interface for a simulation runtime.
  A Runtime manages nodes, events, and logical time.
*)
module type Runtime = sig
  type t

  include Has_spec with type t := t

  module V : Value.S

  module N : Node.S

  type msg = V.t Message.Message.t

  (** Simulation events are typed, semantic simulation steps. *)
  type event = Sim_event.t

  (******************************************)
  (* Control and simulation loop operations *)
  (******************************************)

  val start : t -> unit
  (** Start the simulation, executing continuously until stopped. *)

  val stop : t -> unit
  (** Stop simulation execution (halts loop). *)

  val pause : t -> unit
  (** Alias for [stop]. *)

  val step : t -> unit
  (** Execute a single discrete simulation tick (dispatch due events). *)

  val reset : t -> unit
  (** Reset the simulator to an initial state and empty timeline. *)

  (*******************)
  (* Node management *)
  (*******************)

  val get_nodes : t -> N.t list
  (** Obtain the list of active nodes registered in the simulation runtime. *)

  val add_node_to_sim : t -> N.spec -> N.t
  (** Register a new node defined by its specification. Also registers the [node] with the [event_bus] *)

  (******************************************)
  (* Event API: for managing runtime events *)
  (******************************************)

  val seed_event : t -> Sim_event.t -> unit
  (** Insert a pre‑built event into the scheduler. *)

  val seed_events : t -> Sim_event.t list -> unit
  (** Bulk-insert multiple pre‑built events into the scheduler. *)

  val seed_event_from_spec : t -> Sim_event.spec -> unit
  (** Create and insert an event from its declarative specification. *)

  val seed_events_from_specs : t -> Sim_event.spec list -> unit
  (** Bulk‑insert multiple events from declarative specifications. *)

  val inline_event :
    t -> time:int -> kind:Sim_event.kind -> action:(unit -> unit) -> unit
  (** Convenience helper for programmatically enqueuing immediate events. *)

  val on_event : t -> (Sim_event.t -> unit) -> unit
  (** Subscribe to run‑time notification of dispatched simulation events. *)

  val current_time : t -> Time.t
  (** Retrieve the current logical simulation time. *)

  val print_bus_stats : t -> unit
  (** Print statistics on the simulation event bus for debugging. *)

  (*************************************)
  (* Identifiers and internal counters *)
  (*************************************)

  val next_msg_id : t -> int

  val next_event_id : t -> int

  (*******************************************)
  (* Construction / Configuration of runtime *)
  (*******************************************)

  type spec =
    {max_ticks: int option; deterministic_seed: int option; log_jsonl: bool}
  [@@deriving sexp, yojson]

  val of_spec : spec -> t
end

open Time
open Types
open Sim_event
open Log

module type S = sig
  type t

  val logger_of : t -> Logger.t

  include Has_spec with type t := t

  module V : Value.S

  module N : Node.S

  type msg = V.t Message.Message.t

  type event = Sim_event.t

  (* Runtime Loop Management *)
  val start : t -> unit

  val stop : t -> unit

  val pause : t -> unit

  val step : t -> unit

  val reset : t -> unit

  val is_runnable : t -> bool

  val current_time : t -> Time.t

  (* Node management *)
  val get_nodes : t -> N.t list

  val get_node_by_alias : t -> string -> N.t option

  (* Events API *)

  val seed_event : t -> Sim_event.t -> unit

  val seed_events : t -> Sim_event.t list -> unit

  val seed_events_from_specs : t -> Sim_event.spec list -> unit

  val seed_nodes_from_specs : t -> N.spec list -> unit

  val on_event : t -> (Sim_event.t -> unit) -> unit

  (* Counter state access *)

  val next_msg_id : t -> int

  val next_event_id : t -> int

  (* Struct construction *)
  type spec = {max_ticks: int option} [@@deriving sexp, yojson]

  val of_spec : spec -> t

  val handle_slash_command : t -> string -> unit
end

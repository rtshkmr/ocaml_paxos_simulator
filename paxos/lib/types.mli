open Base

(**
  Core, shared primitive types for the Paxos implementation.

  Intent:
  - Provide canonical definitions for node identifiers and proposal identifiers.
  - Offer a canonical comparator / hashable module for proposal ids to be used
    as map/set keys. This is done using ppx annotations for its ergonomics
  - Keep this module minimal and stable: other interfaces will refer to it.
*)
module Types : sig
  type node_id = int [@@deriving sexp, compare, equal, hash]

  (** Slots are for future multi-paxos implementations*)
  type slot = int [@@deriving sexp, compare, equal]

  (** A proposal id is a pair (counter, node) that gives a total ordering. *)
  type proposal_id = {seq: int  (** sequence counter*); node: node_id}
  [@@deriving sexp, compare, equal, hash]

  (** Explicit topics for which nodes communicate*)
  type topic =
    | Coordination  (** Consensus coordination messages, e.g., Paxos *)
    | Simulation_control  (** Simulator commands controlling nodes *)
    | Gossip  (** Peer-to-peer state propagation *)
    | Metrics  (** Telemetry and monitoring data *)
    | Time  (** Logical time simulation & clock sync msgs *)
  [@@deriving sexp, compare, hash, equal]

  val make_proposal_id : seq:int -> node:node_id -> proposal_id
  (** Convenience builder helpers *)

  val next_proposal_id : proposal_id -> node:node_id -> proposal_id
end

(** Our Time module is a type alias *)
module Time : sig
  (* type t = Time.System.t [@@deriving sexp, compare, equal] *)
  type t = float [@@deriving sexp, compare, equal]
end

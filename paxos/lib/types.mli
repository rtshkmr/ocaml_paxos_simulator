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

  (** When driving consensus, a node would need to assert their own value first.*)
  type 'a paxos_assertion_state = {proposal: proposal_id; value: 'a}
  [@@deriving sexp, compare, equal]

  type 'a paxos_promise = 'a paxos_assertion_state option
  [@@deriving sexp, compare, equal]

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

  val topic_of_str : string -> topic option
end

(** Modules that satisfy this interface define a specific spec type which can be used to generate structs of that module.*)
module type Has_spec = sig
  type t

  type spec

  val of_spec : spec -> t
end

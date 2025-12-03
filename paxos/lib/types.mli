open Base

(**
  Core, shared primitive types for the whole system.

  Intent:
  - Provide canonical definitions for node identifiers and proposal identifiers.

    All of these are comparable and hashable, they may be used as keys for containers like map/set

  - We keep this module minimal and stable: other interfaces will refer to it.
*)
module Types : sig
  type node_id = int [@@deriving sexp, compare, equal, hash, yojson]

  (** Slots are for future multi-paxos implementations*)
  type slot = int [@@deriving sexp, compare, equal]

  (** A proposal id is a pair (counter, node) that gives a total ordering. *)
  type proposal_id = {seq: int  (** sequence counter*); node: node_id}
  [@@deriving sexp, compare, equal, hash, yojson]

  val proposal_id_to_string : proposal_id -> string
  (** String repr of proposal*)

  (** When driving consensus, a node would need to assert their own value first.

    NOTE [semantics]: this is named [assertion] in the context of the paxos protocol in that:
     - peer nodes assert on what they think the value (the state that we desire to seek consensus on) will be
     - the word "assertion" is unrelated to the programming construct "assertion"
  *)
  type 'a paxos_assertion_state = {proposal: proposal_id; value: 'a}
  [@@deriving sexp, compare, equal, yojson]

  (** NOTE [semantics]: this is named [promise] in the context of the paxos protocol in that:
     - acceptors receive promises on what the value (the state that we desire to seek consensus on) will be
      - a promise is a possible assertion. That's why it's optional.*)
  type 'a paxos_promise = 'a paxos_assertion_state option
  [@@deriving sexp, compare, equal, yojson]

  type proposal_id_spec = {seq: int; node: int} [@@deriving sexp, yojson]

  type 'a assertion_spec = {proposal: proposal_id_spec; value: 'a}
  [@@deriving sexp, yojson]

  val assertion_of_spec :
    ('b -> 'a) -> 'b assertion_spec -> 'a paxos_assertion_state

  type 'a promise_spec = 'a assertion_spec option [@@deriving sexp, yojson]

  val promise_of_spec : ('b -> 'a) -> 'b promise_spec -> 'a paxos_promise

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

  val topic_to_str : topic -> string

  val topic_of_str : string -> topic option
end

(** Modules that satisfy this interface define a specific spec type which can be used to generate structs of that module.*)
module type Has_spec = sig
  type t

  type spec [@@deriving sexp, yojson]

  val of_spec : spec -> t
end

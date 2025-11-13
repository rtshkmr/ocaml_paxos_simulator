open Base

module Types = struct
  type node_id = int [@@deriving sexp, compare, equal, hash, yojson]

  type slot = int [@@deriving sexp, compare, equal]

  type proposal_id = {seq: int; node: node_id}
  [@@deriving sexp, compare, equal, hash, yojson]

  (** When driving consensus, a node would need to assert their own value first.*)
  type 'a paxos_assertion_state = {proposal: proposal_id; value: 'a}
  [@@deriving sexp, compare, equal, yojson]

  type 'a paxos_promise = 'a paxos_assertion_state option
  [@@deriving sexp, compare, equal]

  let make_proposal_id ~seq ~node = {seq; node}

  type topic =
    | Coordination  (** Consensus coordination messages, e.g., Paxos *)
    | Simulation_control  (** Simulator commands controlling nodes *)
    | Gossip  (** Peer-to-peer state propagation *)
    | Metrics  (** Telemetry and monitoring data *)
    | Time  (** Logical time simulation & clock sync msgs *)
  [@@deriving sexp, compare, hash, equal]

  let topic_of_str = function
    | "Coordination" ->
        Some Coordination
    | "Simulation_control" ->
        Some Simulation_control
    | "Gossip" ->
        Some Gossip
    | "Metrics" ->
        Some Metrics
    | "Time" ->
        Some Time
    | _ ->
        None
end

module type Has_spec = sig
  type t

  type spec [@@deriving sexp, yojson]

  val of_spec : spec -> t
end

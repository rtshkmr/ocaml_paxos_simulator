open Base

module Types = struct
  type node_id = int [@@deriving sexp, compare, equal, hash]

  type slot = int [@@deriving sexp, compare, equal]

  type proposal_id = {seq: int; node: node_id}
  [@@deriving sexp, compare, equal, hash]

  let make_proposal_id ~seq ~node = {seq; node}

  let next_proposal_id prev ~node = {seq= prev.seq + 1; node}

  type topic =
    | Coordination  (** Consensus coordination messages, e.g., Paxos *)
    | Simulation_control  (** Simulator commands controlling nodes *)
    | Gossip  (** Peer-to-peer state propagation *)
    | Metrics  (** Telemetry and monitoring data *)
    | Time  (** Logical time simulation & clock sync msgs *)
  [@@deriving sexp, compare, hash, equal]
end

open Base

module Types = struct
  type node_id = int [@@deriving sexp, compare, equal, hash]

  type slot = int [@@deriving sexp, compare, equal]

  type proposal_id = {seq: int; node: node_id}
  [@@deriving sexp, compare, equal, hash]

  let make_proposal_id ~seq ~node = {seq; node}

  let next_proposal_id prev ~node = {seq= prev.seq + 1; node}

  type topic = Coordination | Suggestion | Control | Gossip | Metrics
  [@@deriving sexp, compare, equal, hash]
end

module Time = struct
  type t = float [@@deriving sexp, compare, equal]
end

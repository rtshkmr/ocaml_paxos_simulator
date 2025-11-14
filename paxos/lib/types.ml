open Base

module Types = struct
  type node_id = int [@@deriving sexp, compare, equal, hash, yojson]

  type slot = int [@@deriving sexp, compare, equal]

  type proposal_id = {seq: int; node: node_id}
  [@@deriving sexp, compare, equal, hash, yojson]

  let make_proposal_id ~seq ~node = {seq; node}

  type proposal_id_spec = {seq: int; node: int} [@@deriving sexp, yojson]

  (** When driving consensus, a node would need to assert their own value first.*)
  type 'a paxos_assertion_state = {proposal: proposal_id; value: 'a}
  [@@deriving sexp, compare, equal, yojson]

  type 'a assertion_spec = {proposal: proposal_id_spec; value: 'a}
  [@@deriving sexp, yojson]

  let assertion_of_spec a_of_spec ({proposal; value} : 'b assertion_spec) :
      'a paxos_assertion_state =
    let {seq; node} = proposal in
    let value = value |> a_of_spec in
    let assertion : 'a paxos_assertion_state =
      {proposal= make_proposal_id ~seq ~node; value}
    in
    assertion

  type 'a paxos_promise = 'a paxos_assertion_state option
  [@@deriving sexp, compare, equal, yojson]

  type 'a promise_spec = 'a assertion_spec option [@@deriving sexp, yojson]

  let promise_of_spec (a_of_spec : 'b -> 'a) (spec : 'b promise_spec) :
      'a paxos_promise =
    Option.map ~f:(assertion_of_spec a_of_spec) spec

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

open Base
open Types

module type S = sig
  module V : Value.S

  type assertion = V.t Types.paxos_assertion_state [@@deriving sexp, yojson]

  type promise = assertion option [@@deriving sexp, yojson]

  type value = {promised: Types.proposal_id option; accepted: promise}
  [@@deriving sexp, yojson]

  include Has_spec with type t := value

  type proposal_id_spec = {seq: int; node: int} [@@deriving sexp, yojson]

  type assertion_spec = {proposal: proposal_id_spec; value: V.spec}
  [@@deriving sexp, yojson]

  val assertion_of_spec : assertion_spec -> assertion

  type promise_spec = assertion_spec option [@@deriving sexp, yojson]

  val promise_of_spec : promise_spec -> promise

  type spec = {promised: proposal_id_spec option; accepted: promise_spec}
  [@@deriving sexp, yojson]

  val of_spec : spec -> value
end

module Make_acceptor_record (V : Value.S) : S with module V = V = struct
  module V = V

  type assertion = V.t Types.paxos_assertion_state [@@deriving sexp, yojson]

  type promise = assertion option [@@deriving sexp, yojson]

  type value = {promised: Types.proposal_id option; accepted: promise}
  [@@deriving sexp, yojson]

  type proposal_id_spec = {seq: int; node: int} [@@deriving sexp, yojson]

  let proposal_of_spec ({seq; node} : proposal_id_spec) : Types.proposal_id =
    Types.make_proposal_id ~seq ~node

  type assertion_spec = {proposal: proposal_id_spec; value: V.spec}
  [@@deriving sexp, yojson]

  let assertion_of_spec ({proposal; value} : assertion_spec) :
      V.t Types.paxos_assertion_state =
    {proposal= proposal |> proposal_of_spec; value= V.of_spec value}

  type promise_spec = assertion_spec option [@@deriving sexp, yojson]

  let promise_of_spec (p : promise_spec) : promise =
    p |> Option.map ~f:assertion_of_spec

  type spec = {promised: proposal_id_spec option; accepted: promise_spec}
  [@@deriving sexp, yojson]

  let of_spec ({promised; accepted} : spec) : value =
    let promised = promised |> Option.map ~f:proposal_of_spec in
    let accepted = accepted |> promise_of_spec in
    {promised; accepted}
end

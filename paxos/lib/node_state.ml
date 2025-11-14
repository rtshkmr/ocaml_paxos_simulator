[@@@ocaml.warning "-27-32-37"]

open Base
open Types

module type S = sig
  module V : Value.S

  type assertion = V.t Types.paxos_assertion_state

  type promise = V.t Types.paxos_promise

  type acceptor_record = {promised: promise; accepted: promise}
  [@@deriving sexp, yojson]

  type nack = {rejected_assertion: assertion; hint: promise} [@@deriving sexp]

  type waiting_for_promise_state =
    { assertion: assertion
    ; promises_received: promise list
    ; nacks_received: nack list }
  [@@deriving sexp]

  type proposer_accepting_state =
    {assertion: assertion; acks: Types.node_id list; nacks_received: nack list}
  [@@deriving sexp]

  type proposer_state =
    | ProposerInactive
    | Idle
    | Preparing of assertion
    | WaitingForPromises of waiting_for_promise_state
    | ProposerAccepting of proposer_accepting_state
    | Decided of V.t
  [@@deriving sexp]

  type acceptor_state = AcceptorInactive | Idle | Accepting of acceptor_record

  type learner_state = Learned of assertion list [@@deriving sexp]

  type role_state =
    {proposer: proposer_state; acceptor: acceptor_state; learner: learner_state}
  [@@deriving sexp, yojson]

  type _ role_selector =
    | Proposer : proposer_state role_selector
    | Acceptor : acceptor_state role_selector
    | Learner : learner_state role_selector

  val get_role : 'a. role_state -> 'a role_selector -> 'a

  val set_role : 'a. role_state -> 'a role_selector -> 'a -> role_state

  val idle_of : unit -> role_state

  val inactive_of : unit -> role_state

  type quorum_result =
    | NotReached
    | MajorityNacks of promise
    | MajorityGrants of assertion

  val is_quorum_reached : role_state -> int -> quorum_result

  include Has_spec with type t := role_state
end

module Make_node_state (V : Value.S) = struct
  module V = V

  type assertion = V.t Types.paxos_assertion_state [@@deriving sexp, yojson]

  type promise = V.t Types.paxos_promise [@@deriving sexp, yojson]

  type nack = {rejected_assertion: assertion; hint: promise}
  [@@deriving sexp, yojson]

  type acceptor_record = {promised: promise; accepted: promise}
  [@@deriving sexp, yojson]

  type waiting_for_promise_state =
    { assertion: assertion
    ; promises_received: promise list
    ; nacks_received: nack list }
  [@@deriving sexp, yojson]

  type proposer_accepting_state =
    {assertion: assertion; acks: Types.node_id list; nacks_received: nack list}
  [@@deriving sexp, yojson]

  type proposer_state =
    | ProposerInactive
        (** this state encodes it's unavailability. When a node is inactivated, it is effectively killed -- it must look to its storage to resume partitipation thereafter *)
    | Idle
        (** An node may be idle to indicate that it can be a valid participant (by initiating a proposal)*)
    | Preparing of assertion
        (** A node that is initiating a proposal will be in the preparing state. It will be ready to .*)
    | WaitingForPromises of waiting_for_promise_state
        (** A node that is gathering responses to their proposal and is waiting to reach a quorum of responses.*)
    | ProposerAccepting of proposer_accepting_state
        (** A node that has suggested *)
    | Decided of V.t
        (** A node that has decided on the value that consensus has been achieved for*)
  [@@deriving sexp, yojson]

  type acceptor_state = AcceptorInactive | Idle | Accepting of acceptor_record
  [@@deriving sexp, yojson]

  type learner_state = Learned of assertion list [@@deriving sexp, yojson]

  type role_state =
    {proposer: proposer_state; acceptor: acceptor_state; learner: learner_state}
  [@@deriving sexp, yojson]

  (* TODO: idle routine to be done *)
  let idle_of () = {proposer= Idle; acceptor= Idle; learner= Learned []}

  (* TODO: inactivate routine *)
  let inactive_of () =
    {proposer= ProposerInactive; acceptor= AcceptorInactive; learner= Learned []}

  (** GADT to encode which role and its sub-state type
        This encodes the association between a constructor (Proposer, Acceptor, Learner) and its precise sub-state type.
    *)
  type _ role_selector =
    | Proposer : proposer_state role_selector
    | Acceptor : acceptor_state role_selector
    | Learner : learner_state role_selector

  (** Polymorphic role_state accessor *)
  let get_role : type a. role_state -> a role_selector -> a =
   fun rs sel ->
    match sel with
    | Proposer ->
        rs.proposer
    | Acceptor ->
        rs.acceptor
    | Learner ->
        rs.learner

  (** Polymorphic role_state setter *)
  let set_role : type a. role_state -> a role_selector -> a -> role_state =
   fun rs sel v ->
    match sel with
    | Proposer ->
        {rs with proposer= v}
    | Acceptor ->
        {rs with acceptor= v}
    | Learner ->
        {rs with learner= v}

  type quorum_result =
    | NotReached
    | MajorityNacks of promise
    | MajorityGrants of assertion

  (* TODO Verify this quorum determination below for the WaitingForPromises is correct *)

  (** Consider the highest proposal_id from all promises received.
          case 1: it's None, use proposers own value
          case 2: it's Some, pick associated value for highest proposal id
    *)
  let is_quorum_reached_on_promise_wait
      {promises_received; nacks_received; assertion} cluster_size =
    let threshold = (cluster_size / 2) + 1 in
    if List.length promises_received >= threshold then
      let chosen_value =
        List.fold ~init:assertion
          ~f:(fun acc promise_rcvd ->
            match (acc, promise_rcvd) with
            | acc, None ->
                acc
            | ( {proposal= best_proposal; value= best_val}
              , Some
                  ( {proposal= prev_accepted_proposal; value= prev_accepted_val}
                    as prev_promise ) ) ->
                if
                  Types.compare_proposal_id prev_accepted_proposal best_proposal
                  > 0
                then prev_promise
                else acc )
          promises_received
      in
      MajorityGrants chosen_value
    else if List.length nacks_received >= threshold then
      let hint =
        List.fold ~init:None
          ~f:(fun acc {rejected_assertion; hint} ->
            match (acc, hint) with
            | _, None ->
                acc
            | None, Some prev_accepted_assertion ->
                hint
            | Some acc_assertion, Some prev_accepted_assertion ->
                if
                  Types.compare_proposal_id prev_accepted_assertion.proposal
                    acc_assertion.proposal
                  > 0
                then hint
                else acc )
          nacks_received
      in
      MajorityNacks hint
    else NotReached

  let is_quorum_reached_on_proposer_accepting_wait
      {acks; nacks_received; assertion} cluster_size =
    let threshold = (cluster_size / 2) + 1 in
    if List.length acks >= threshold then MajorityGrants assertion
    else if List.length nacks_received >= threshold then
      let hint =
        List.fold ~init:None
          ~f:(fun acc {rejected_assertion; hint} ->
            match (acc, hint) with
            | _, None ->
                acc
            | None, Some prev_accepted_assertion ->
                hint
            | Some acc_assertion, Some prev_accepted_assertion ->
                if
                  Types.compare_proposal_id prev_accepted_assertion.proposal
                    acc_assertion.proposal
                  > 0
                then hint
                else acc )
          nacks_received
      in
      MajorityNacks hint
    else NotReached

  let is_quorum_reached rs cluster_size =
    match get_role rs Proposer with
    | WaitingForPromises wfp ->
        is_quorum_reached_on_promise_wait wfp cluster_size
    | ProposerAccepting pa ->
        is_quorum_reached_on_proposer_accepting_wait pa cluster_size
    | _ ->
        assert false
  (* "We can only check for quorum reached on a nodes if that nodes is one of the states: [WaitingForPromises, ProposerAccepting]" *)

  type acceptor_record_spec =
    {promised: V.t Types.promise_spec; accepted: V.t Types.promise_spec}
  [@@deriving sexp, yojson]

  let acceptor_record_of_spec ({promised; accepted} : acceptor_record_spec) :
      acceptor_record =
    { promised= promised |> Types.promise_of_spec Fn.id
    ; accepted= accepted |> Types.promise_of_spec Fn.id }

  (* ==== specs and of_specs *)
  type nack_spec =
    {rejected_assertion: V.t Types.assertion_spec; hint: V.t Types.promise_spec}
  [@@deriving sexp, yojson]

  let nack_of_spec ({rejected_assertion; hint} : nack_spec) : nack =
    let ra = rejected_assertion |> Types.assertion_of_spec Fn.id in
    let h = hint |> Types.promise_of_spec Fn.id in
    {rejected_assertion= ra; hint= h}

  type waiting_for_promise_state_spec =
    { assertion: V.t Types.assertion_spec
    ; promises_received: V.t Types.promise_spec list
    ; nacks_received: nack_spec list }
  [@@deriving sexp, yojson]

  let waiting_for_promise_state_of_spec
      ({assertion; promises_received; nacks_received} :
        waiting_for_promise_state_spec ) : waiting_for_promise_state =
    { assertion= assertion |> Types.assertion_of_spec Fn.id
    ; promises_received=
        promises_received |> List.map ~f:(Types.promise_of_spec Fn.id)
    ; nacks_received= nacks_received |> List.map ~f:nack_of_spec }

  type proposer_accepting_state_spec =
    { assertion: V.t Types.assertion_spec
    ; acks: int list
    ; nacks_received: nack_spec list }
  [@@deriving sexp, yojson]

  let proposer_accepting_state_of_spec
      ({assertion; acks; nacks_received} : proposer_accepting_state_spec) :
      proposer_accepting_state =
    { assertion= assertion |> Types.assertion_of_spec Fn.id
    ; acks
    ; nacks_received= nacks_received |> List.map ~f:nack_of_spec }

  type proposer_state_spec =
    | ProposerInactive_spec
    | Idle_spec
    | Preparing_spec of V.t Types.assertion_spec
    | WaitingForPromises_spec of waiting_for_promise_state_spec
    | ProposerAccepting_spec of proposer_accepting_state_spec
    | Decided_spec of V.spec
  [@@deriving sexp, yojson]

  let proposer_state_of_spec = function
    | ProposerInactive_spec ->
        ProposerInactive
    | Idle_spec ->
        Idle
    | Preparing_spec assertion_spec ->
        Preparing (assertion_spec |> Types.assertion_of_spec Fn.id)
    | WaitingForPromises_spec wfp_spec ->
        WaitingForPromises (wfp_spec |> waiting_for_promise_state_of_spec)
    | ProposerAccepting_spec pas_spec ->
        ProposerAccepting (pas_spec |> proposer_accepting_state_of_spec)
    | Decided_spec v_spec ->
        Decided (v_spec |> V.of_spec)

  type acceptor_state_spec =
    | AcceptorInactive_spec
    | Idle_spec
    | Accepting_spec of acceptor_record_spec
  [@@deriving sexp, yojson]

  let acceptor_state_of_spec = function
    | AcceptorInactive_spec ->
        AcceptorInactive
    | Idle_spec ->
        Idle
    | Accepting_spec asp ->
        Accepting (asp |> acceptor_record_of_spec)

  type learner_state_spec = Learned_spec of V.t Types.assertion_spec list
  [@@deriving sexp, yojson]

  let learner_state_of_spec
      (Learned_spec (aspecs : V.t Types.assertion_spec list)) =
    Learned (aspecs |> List.map ~f:(Types.assertion_of_spec Fn.id))

  type role_state_spec =
    { proposer: proposer_state_spec
    ; acceptor: acceptor_state_spec
    ; learner: learner_state_spec }
  [@@deriving sexp, yojson]

  let role_state_of_spec ({proposer; acceptor; learner} : role_state_spec) :
      role_state =
    { proposer= proposer |> proposer_state_of_spec
    ; acceptor= acceptor |> acceptor_state_of_spec
    ; learner= learner |> learner_state_of_spec }

  type spec = role_state_spec [@@deriving sexp, yojson]

  let of_spec = role_state_of_spec
end

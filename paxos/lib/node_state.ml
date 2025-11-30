open Base
open Types

module type S = sig
  module V : Value.S

  (** NOTE [semantics]: this is named [assertion] in the context of the paxos protocol in that:
     - peer nodes assert on what they think the value (the state that we desire to seek consensus on) will be
     - the word "assertion" is unrelated to the programming construct "assertion"
  *)
  type assertion = V.t Types.paxos_assertion_state [@@deriving sexp, yojson]

  (** NOTE [semantics]: this is named [promise] in the context of the paxos protocol in that:
     - acceptors receive promises on what the value (the state that we desire to seek consensus on) will be
      - a promise is a possible assertion. That's why it's optional.*)
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
        (** A node that is initiating a proposal will be in the preparing state.*)
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

  val is_inactive : role_state -> bool

  type _ role_selector =
    | Proposer : proposer_state role_selector
    | Acceptor : acceptor_state role_selector
    | Learner : learner_state role_selector

  val get_role : 'a. role_state -> 'a role_selector -> 'a

  val set_role : 'a. role_state -> 'a role_selector -> 'a -> role_state

  val idle_of : role_state -> role_state

  val inactive_of : role_state -> role_state

  val current_promise : role_state -> promise

  val last_accepted_promise : role_state -> promise

  val is_proposal_permissible : role_state -> Types.proposal_id -> bool

  val is_suggestion_acceptable : role_state -> Types.proposal_id -> bool

  type quorum_result =
    | NotReached
    | MajorityNacks of promise
    | MajorityGrants of assertion

  val is_quorum_reached : int -> role_state -> quorum_result

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

  let is_inactive {acceptor; proposer; _} =
    match (acceptor, proposer) with
    | AcceptorInactive, _ | _, ProposerInactive ->
        true
    | _ ->
        false

  let state_to_str s =
    s |> sexp_of_role_state |> Sexplib.Sexp.to_string_hum ~indent:4

  let init_state () : role_state =
    {proposer= Idle; acceptor= Idle; learner= Learned []}

  let idle_of rs = {rs with proposer= Idle; acceptor= Idle}

  let inactive_of rs =
    {rs with proposer= ProposerInactive; acceptor= AcceptorInactive}

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

  let select_highest_hint nacks : promise =
    List.fold nacks ~init:None ~f:(fun acc {hint; _} ->
        match (hint, acc) with
        | None, _ ->
            acc
        | Some h, None ->
            Some h
        | Some h, Some a ->
            if Types.compare_proposal_id h.proposal a.proposal > 0 then Some h
            else acc )

  let select_highest_accepted ~(default : assertion) (promises : promise list) =
    promises
    |> List.fold ~init:default ~f:(fun best promise ->
           match promise with
           | None ->
               best
           | Some ({proposal; _} as accepted) ->
               if Types.compare_proposal_id proposal best.proposal > 0 then
                 accepted
               else best )

  let current_promise ({acceptor; _} : role_state) =
    match acceptor with
    | Idle ->
        (* TODO [LOG] should we log this out? *)
        (* log_decision node *)
        (*   alias *)
        (*   (Printf.sprintf *)
        (*      "%s Acceptor was idle; no current promised / previously \ *)
          (*       accepted to report. Carrying on..." *)
        (*      alias ) ; *)
        None
    | Accepting {promised; _} ->
        promised
    | _ ->
        assert false

  let last_accepted_promise ({acceptor; _} : role_state) =
    match acceptor with
    | Idle ->
        None
    | Accepting {accepted; _} ->
        accepted
    | _ ->
        assert false

  (* TODO Verify this quorum determination below for the WaitingForPromises is correct *)

  (** Consider the highest proposal_id from all promises received.
          case 1: it's None, use proposers own value
          case 2: it's Some, pick associated value for highest proposal id
    *)
  let is_quorum_reached_on_promise_wait cluster_size
      {promises_received; nacks_received; assertion} =
    let threshold = (cluster_size / 2) + 1 in
    if promises_received |> List.length >= threshold then
      promises_received
      |> select_highest_accepted ~default:assertion
      |> MajorityGrants
    else if nacks_received |> List.length >= threshold then
      nacks_received |> select_highest_hint |> MajorityNacks
    else NotReached

  let is_quorum_reached_on_proposer_accepting_wait cluster_size
      {acks; nacks_received; assertion} =
    let threshold = (cluster_size / 2) + 1 in
    if acks |> List.length >= threshold then assertion |> MajorityGrants
    else if nacks_received |> List.length >= threshold then
      nacks_received |> select_highest_hint |> MajorityNacks
    else NotReached

  let is_quorum_reached cluster_size rs =
    match Proposer |> get_role rs with
    | WaitingForPromises wfp ->
        wfp |> is_quorum_reached_on_promise_wait cluster_size
    | ProposerAccepting pa ->
        pa |> is_quorum_reached_on_proposer_accepting_wait cluster_size
    | _ ->
        failwith
          "is_quorum_reached called on non-proposer state (must be \
           WaitingForPromises or ProposerAccepting)"

  let is_proposal_permissible role_state proposal =
    role_state |> current_promise
    |> Option.value_map ~default:true
         ~f:(fun
             ({proposal= promised_proposal; _} : V.t Types.paxos_assertion_state)
           -> Types.compare_proposal_id proposal promised_proposal >= 0 )

  let is_suggestion_acceptable role_state proposal =
    (* NOTE [INVARIANT] : when checking for acceptable suggestion, the node would have had granted permission prior in phase 1 of the paxos protocol *)
    role_state |> current_promise |> Option.value_exn
    |> fun ({proposal= promised_proposal; _} : V.t Types.paxos_assertion_state)
       -> Types.compare_proposal_id proposal promised_proposal >= 0

  type acceptor_record_spec =
    {promised: V.t Types.promise_spec; accepted: V.t Types.promise_spec}
  [@@deriving sexp, yojson]

  let acceptor_record_of_spec {promised; accepted} : acceptor_record =
    { promised= promised |> Types.promise_of_spec Fn.id
    ; accepted= accepted |> Types.promise_of_spec Fn.id }

  (* ==== specs and of_specs *)
  type nack_spec =
    {rejected_assertion: V.t Types.assertion_spec; hint: V.t Types.promise_spec}
  [@@deriving sexp, yojson]

  let nack_of_spec {rejected_assertion; hint} : nack =
    { rejected_assertion= rejected_assertion |> Types.assertion_of_spec Fn.id
    ; hint= hint |> Types.promise_of_spec Fn.id }

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
        assertion_spec |> Types.assertion_of_spec Fn.id |> Preparing
    | WaitingForPromises_spec wfp_spec ->
        wfp_spec |> waiting_for_promise_state_of_spec |> WaitingForPromises
    | ProposerAccepting_spec pas_spec ->
        pas_spec |> proposer_accepting_state_of_spec |> ProposerAccepting
    | Decided_spec v_spec ->
        v_spec |> V.of_spec |> Decided

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
    | Accepting_spec spec ->
        spec |> acceptor_record_of_spec |> Accepting

  type learner_state_spec = Learned_spec of V.t Types.assertion_spec list
  [@@deriving sexp, yojson]

  let learner_state_of_spec
      (Learned_spec (aspecs : V.t Types.assertion_spec list)) =
    aspecs |> List.map ~f:(Types.assertion_of_spec Fn.id) |> Learned

  type role_state_spec =
    { proposer: proposer_state_spec
    ; acceptor: acceptor_state_spec
    ; learner: learner_state_spec }
  [@@deriving sexp, yojson]

  let role_state_of_spec {proposer; acceptor; learner} : role_state =
    { proposer= proposer |> proposer_state_of_spec
    ; acceptor= acceptor |> acceptor_state_of_spec
    ; learner= learner |> learner_state_of_spec }

  type spec = role_state_spec [@@deriving sexp, yojson]

  let of_spec = role_state_of_spec
end

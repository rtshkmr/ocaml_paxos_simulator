(* [subroutines] unit-level tests for the core Paxos invariants.

    These tests exercise the pure logic in {!Node_state.Make_node_state}
    and {!Types} without spinning up a full simulation.  They are the
    cheapest tests to write and the fastest to run.

    Invariants under test:
    1. Quorum thresholds   — is_quorum_reached returns the right variant
    2. Proposal ordering   — is_proposal_permissible / is_suggestion_acceptable
*)
open Base
open Paxos
module VS = Value_string.Value_string
module NS = Node_state.Make_node_state (VS)
module T = Types.Types

(** Helps us create [NS.V.t] (expected to just be a value-string) using the
    sexps workaround. [NS.V.t] is abstract from outside the functor, even though
    we know it's a [ string ] underneath, OCaml won't unify the two. The way to
    get a value of type [ NS.V.t ] without touching [ spec ] is via [ sexp ],
    since [t] derives it. *)
let make_vs s = NS.V.t_of_sexp (Sexplib.Sexp.Atom s)

let pid ?(node = 1) seq = T.make_proposal_id ~seq ~node

let assertion ?(node = 1) seq value : NS.assertion =
  { T.proposal = pid ~node seq; value }

(** Builds a role_state whose proposer is in WaitingForPromises with the given
    [promises] and [nacks]. Acceptor/learner remain Idle / empty. *)
let waiting_state ?(promises = []) ?(nacks = []) seq value =
  let rs = NS.init_state () in
  let a = assertion seq value in
  let wfp =
    NS.WaitingForPromises
      { assertion = a; promises_received = promises; nacks_received = nacks }
  in
  NS.set_role rs NS.Proposer wfp

(** A blank "promise" meaning the acceptor had no prior accepted value. *)
let blank_promise : NS.promise = None

(** A non-blank promise carrying the given value. *)
let promise_with_value seq s : NS.promise = Some (assertion seq (make_vs s))

(** Builds a nack record from a rejected_assertion and an optional hint. *)
let nack_of seq s hint : NS.nack =
  { NS.rejected_assertion = assertion seq (make_vs s); hint }

(** Renders a quorum_result to a short string for expect output. *)
let quorum_result_to_string = function
  | NS.NotReached -> "NotReached"
  | NS.MajorityGrants a ->
      let s = a.T.value |> VS.to_string in
      Printf.sprintf "MajorityGrants(%s)" s
  | NS.MajorityNacks _ -> "MajorityNacks"

(* 1: Quorum phase 1: WaitingForPromises phase *)
let%expect_test "quorum/phase1: 0 promises in 3-node cluster → NotReached" =
  let rs = waiting_state 1 ("foo" |> make_vs) in
  rs |> NS.is_quorum_reached 3 |> quorum_result_to_string |> Stdio.print_string;
  [%expect {| NotReached |}]

let%expect_test
    "quorum/phase1: 1 promise in 3-node cluster → NotReached (below majority)" =
  let rs = waiting_state ~promises:[ blank_promise ] 1 ("foo" |> make_vs) in
  rs |> NS.is_quorum_reached 3 |> quorum_result_to_string |> Stdio.print_string;
  [%expect {| NotReached |}]

let%expect_test "quorum/phase1: 2 promises in 3-node cluster → MajorityGrants" =
  (* threshold = (3/2)+1 = 2.  Two blank promises → own value wins. *)
  let rs =
    waiting_state ~promises:[ blank_promise; blank_promise ] 1 ("foo" |> make_vs)
  in
  rs |> NS.is_quorum_reached 3 |> quorum_result_to_string |> Stdio.print_string;
  [%expect {| MajorityGrants(foo) |}]

let%expect_test "quorum/phase1: majority nacks → MajorityNacks" =
  let nacks =
    [ nack_of 1 "foo" blank_promise; nack_of 1 "foo" blank_promise ]
  in
  let rs = waiting_state ~nacks 1 ("foo" |> make_vs) in
  rs |> NS.is_quorum_reached 3 |> quorum_result_to_string |> Stdio.print_string;
  [%expect {| MajorityNacks |}]

let%expect_test "quorum/phase1: highest accepted value wins over proposer's own"
    =
  (* An acceptor that previously accepted (seq=5, "bar") sends back that
     promise.  The proposer originally wanted "foo" at seq=1.  After quorum,
     the chosen value should be "bar" (higher proposal_id wins). *)
  let own_assertion = "foo" |> make_vs in
  let higher_promise = promise_with_value 5 "bar" in
  let rs =
    waiting_state ~promises:[ higher_promise; blank_promise ] 1 own_assertion
  in
  rs |> NS.is_quorum_reached 3 |> quorum_result_to_string |> Stdio.print_string;
  [%expect {| MajorityGrants(bar) |}]

let%expect_test "quorum/phase1: 5-node cluster needs 3 for majority" =
  (* in a 5-node cluster: majority = 3 *)
  let rs2 =
    waiting_state ~promises:[ blank_promise; blank_promise ] 1 ("q" |> make_vs)
  in
  let rs3 =
    waiting_state
      ~promises:[ blank_promise; blank_promise; blank_promise ]
      1 ("q" |> make_vs)
  in
  let r2 = rs2 |> NS.is_quorum_reached 5 |> quorum_result_to_string in
  let r3 = rs3 |> NS.is_quorum_reached 5 |> quorum_result_to_string in
  Stdio.printf "2 promises: %s\n3 promises: %s\n" r2 r3;
  [%expect
    {|
    2 promises: NotReached
    3 promises: MajorityGrants(q)
    |}]

(** 2. Quorum phase 2: ProposerAccepting phase *)

(** Builds a role_state whose proposer is in ProposerAccepting. *)
let accepting_state ?(acks = []) ?(nacks = []) seq value : NS.role_state =
  let rs = NS.init_state () in
  let a = assertion seq value in
  let pa =
    NS.ProposerAccepting { assertion = a; acks; nacks_received = nacks }
  in
  NS.set_role rs NS.Proposer pa

let%expect_test "quorum/phase2: 2 acks in 3-node cluster → MajorityGrants" =
  let rs =
    accepting_state ~acks:[ 1; 2 ] 1 ("we riot today; not tomorrow" |> make_vs)
  in
  rs |> NS.is_quorum_reached 3 |> quorum_result_to_string |> Stdio.print_string;
  [%expect {| MajorityGrants(we riot today; not tomorrow) |}]

let%expect_test "quorum/phase2: 1 ack in 3-node cluster → NotReached" =
  let vs = "we riot today; not tomorrow" |> make_vs in
  let rs = accepting_state ~acks:[ 1 ] 1 vs in
  rs |> NS.is_quorum_reached 3 |> quorum_result_to_string |> Stdio.print_string;
  [%expect {| NotReached |}]

(** 3: Proposal permissibility [notes] 1. we just use pid values in the examples
    below because we just treat it as self receive. *)
let show_perm_result r =
  Stdio.print_string (if r then "permissible" else "rejected")

let%expect_test "permissibility: idle acceptor permits any proposal" =
  (* An Idle acceptor (no prior promise) should accept any proposal_id. *)
  let rs = NS.init_state () |> NS.idle_of in
  pid 1 |> NS.is_proposal_permissible rs |> show_perm_result;
  [%expect {| permissible |}]

let%expect_test
    "permissibility: acceptor rejects proposal strictly below promised" =
  (* Acceptor already promised seq=5; a new request at seq=3 must be rejected. *)
  let promised = assertion 5 ("bar" |> make_vs) in
  let accepting_rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      ({ promised = Some promised; accepted = None } |> NS.Accepting)
  in
  pid 3 |> NS.is_proposal_permissible accepting_rs |> show_perm_result;
  [%expect {| rejected |}]

let%expect_test
    "permissibility: acceptor rejects proposal strictly below promised even \
     from higher node" =
  (* Acceptor already promised seq=5; a new request at seq=3 must be rejected, even if it's from a higher id-ed node. *)
  let promised = assertion ~node:2 5 ("bar" |> make_vs) in
  let accepting_rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      ({ promised = Some promised; accepted = None } |> NS.Accepting)
  in
  let incoming = pid ~node:5 3 in
  incoming |> NS.is_proposal_permissible accepting_rs |> show_perm_result;
  [%expect {| rejected |}]

let%expect_test
    "permissibility: accepts proposal if same seq number AND from higher node \
     id" =
  (* Acceptor already promised seq=5 from node 1; a new request at seq=5 but from a higher node id must be accepted because of the total ordering on proposal_id. *)
  let promised = assertion ~node:1 5 ("bar" |> make_vs) in
  let accepting_rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      ({ promised = Some promised; accepted = None } |> NS.Accepting)
  in
  let incoming = pid ~node:5 5 in
  incoming |> NS.is_proposal_permissible accepting_rs |> show_perm_result;
  [%expect {| permissible |}]

let%expect_test
    "permissibility: acceptor permits proposal equal to promised (and equal)" =
  (* Same seq as promised → should be permitted (>= check). *)
  let promised = assertion 5 ("bar" |> make_vs) in
  let accepting_rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      (NS.Accepting { promised = Some promised; accepted = None })
  in
  pid 5 |> NS.is_proposal_permissible accepting_rs |> show_perm_result;
  [%expect {| permissible |}]

let%expect_test
    "permissibility: acceptor permits proposal strictly above promised" =
  let promised = assertion 5 ("bar" |> make_vs) in
  let accepting_rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      (NS.Accepting { promised = Some promised; accepted = None })
  in
  pid 7 |> NS.is_proposal_permissible accepting_rs |> show_perm_result;
  [%expect {| permissible |}]

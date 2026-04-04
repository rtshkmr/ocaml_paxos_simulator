(* [subroutines] unit-level tests for the core Paxos invariants.

    These tests exercise the pure logic in {!Node_state.Make_node_state}
    and {!Types} without spinning up a full simulation.  They are the
    cheapest tests to write and the fastest to run.

    Invariants under test:
    1. Quorum thresholds   -- is_quorum_reached returns the right variant
    2. Proposal ordering   -- is_proposal_permissible / is_suggestion_acceptable
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

(* %%%%%%%%  [ Phase 1 quorum -- counting promises ] %%%%%%%%%%%%%

    In phase 1, a proposer broadcasts a permission request and waits to hear
    back from a majority of acceptors. Each acceptor replies with either a
    promise (granting permission) or a nack (refusing it).

    The quorum threshold is a strict majority: floor(n/2) + 1. For a 3-node
    cluster that is 2; for a 5-node cluster it is 3. The threshold is
    intentionally the same for both grants and nacks -- a majority of either
    settles the round.

    [is_quorum_reached] returns one of three variants:
    - [NotReached]      -- not enough responses yet to decide anything
    - [MajorityGrants]  -- enough promises to proceed to phase 2;
                          carries the value the proposer must use (see
                          the value-adoption section below for why this
                          may differ from the proposer's own preference)
    - [MajorityNacks]   -- enough refusals to abort this round

    The tests below verify the threshold arithmetic and all three outcomes.
    They use hand-constructed role_states so they run without a simulator.
*)

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
  (* Nacks are first-class: a majority of refusals is just as decisive as a
     majority of grants. The proposer must abort rather than keep waiting. *)
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
  (* The threshold formula scales correctly: floor(5/2)+1 = 3.
     Two promises are insufficient; three cross the line. *)
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

(* %%%%%%%%  [ Phase 2 quorum -- counting accepted messages ] %%%%%%%%%%%%%

    Phase 2 begins once a proposer has gathered a majority of promises. It
    sends a suggestion (carrying the adopted value) to all acceptors, and waits
    for them to respond with Accepted messages.

    The quorum threshold is identical to phase 1 -- a strict majority -- but the
    evidence being counted is different. Where phase 1 counts promises, phase 2
    counts acks: node_ids of acceptors that confirmed they accepted the
    suggestion. The proposer transitions to Decided once that majority is
    reached.

    A subtlety worth noting: acks are de-duplicated by node_id. Receiving two
    Accepted messages from the same peer must not count as two votes -- the
    implementation guards against this, and it matters for correctness in the
    presence of network retries or duplicated messages.

    The tests below verify the threshold arithmetic for this phase using
    hand-constructed ProposerAccepting states.
*)

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

(* %%%%%%%%  [ Proposal permissibility -- the acceptor's promise ] %%%%%%%%%%%%%

    When an acceptor receives a permission request in phase 1, it consults its
    own history before deciding whether to grant or refuse. The rule is simple:
    an acceptor will only grant permission to a proposal_id
    that is greater than or equal to any proposal_id it has previously promised.

    This is the monotonicity property of acceptor promises. Once an acceptor
    commits to a higher round, it implicitly invalidates all lower ones. A
    proposer that arrives late, after a competing leader has already collected
    promises from a majority at a higher id, will find all those acceptors
    locked out and must retry with a fresh, higher proposal_id of its own.

    Proposal ids have a total ordering defined over (seq, node_id) pairs. The
    node_id tiebreaker matters: two proposals with the same sequence number from
    different nodes are not equal, and the one from the higher node_id wins.
    This ensures the ordering is total even under concurrent proposals.

    An idle acceptor (no prior promise) grants permission to anything -- it has
    made no commitments yet. This is the clean-slate case every new cluster
    starts in.

    The tests below cover: idle grants, rejections below the
    promise boundary, and the boundary cases where the tiebreaker activates.
*)

let show_perm_result r =
  Stdio.print_string (if r then "permissible" else "rejected")

let%expect_test "permissibility: idle acceptor permits any proposal" =
  (* An Idle acceptor (no prior promise) should accept any proposal_id.
     There is no history to protect, so the gate is always open. *)
  let rs = NS.init_state () |> NS.idle_of in
  pid 1 |> NS.is_proposal_permissible rs |> show_perm_result;
  [%expect {| permissible |}]

let%expect_test
    "permissibility: acceptor rejects proposal strictly below promised" =
  (* Acceptor already promised seq=5; a new request at seq=3 must be rejected.
     The acceptor has implicitly committed to ignoring anything lower. *)
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
  (* The rejection is based on the full (seq, node_id) ordering, not just seq.
     A request at seq=3 from node=5 still loses to a promise at seq=5 from
     node=2, because seq dominates the comparison. The node tiebreaker only
     activates when sequence numbers are equal. *)
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
  (* When sequence numbers are equal the node_id tiebreaker decides.
     Acceptor promised seq=5 from node=1; incoming is seq=5 from node=5.
     Node 5 > node 1 so the total order puts the incoming proposal higher,
     and the acceptor must grant permission. *)
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
  (* Same seq, same node -- the >= boundary includes equality.
     A proposer retrying its own proposal must not be locked out. *)
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
  (* A new leader with a higher proposal_id supersedes the prior promise.
     The acceptor updates its commitment and moves on. *)
  let promised = assertion 5 ("bar" |> make_vs) in
  let accepting_rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      (NS.Accepting { promised = Some promised; accepted = None })
  in
  pid 7 |> NS.is_proposal_permissible accepting_rs |> show_perm_result;
  [%expect {| permissible |}]

(* %%%%%%%%  [ value adoption, safety heart of paxos phase 1 ] %%%%%%%%%%%%%

    When a proposer collects a majority of promises, it does not get to
    propose its own preferred value freely. It must inspect what acceptors
    have previously accepted and adopt the value attached to the highest
    proposal_id seen. Only if every acceptor returns a blank promise (none
    previously accepted anything) may the proposer use its own value.

    This is the mechanism that prevents two proposers from deciding different
    values across separate rounds. If an earlier round partially succeeded --
    some acceptors accepted value V -- then any future proposer that reaches
    quorum is guaranteed to learn about V and carry it forward.
*)

let%expect_test
    "value-adoption: all blank promises → proposer keeps its own value" =
  (* Baseline: no acceptor has previously accepted anything.
     The proposer's own value should survive unchanged. *)
  let rs =
    waiting_state
      ~promises:[ blank_promise; blank_promise ]
      1
      (make_vs "my-preferred-value")
  in
  (match NS.is_quorum_reached 3 rs with
  | NS.MajorityGrants a ->
      Stdio.printf "adopted: %s\n" (NS.V.to_string a.T.value)
  | _ -> Stdio.print_string "no quorum\n");
  [%expect {| adopted: my-preferred-value |}]

let%expect_test
    "value-adoption: single prior acceptance overrides proposer's own value" =
  (* One acceptor previously accepted "already-agreed" at seq=5.
     Our proposer is at seq=1 and wants "my-preferred-value".
     The promise from that acceptor carries the prior accepted value.
     The proposer MUST abandon its preference and adopt "already-agreed". *)
  let prior = promise_with_value 5 "already-agreed" in
  let rs =
    waiting_state ~promises:[ prior; blank_promise ] 1
      (make_vs "my-preferred-value")
  in
  (match NS.is_quorum_reached 3 rs with
  | NS.MajorityGrants a ->
      Stdio.printf "adopted: %s\n" (NS.V.to_string a.T.value)
  | _ -> Stdio.print_string "no quorum\n");
  [%expect {| adopted: already-agreed |}]

let%expect_test
    "value-adoption: highest proposal_id wins across competing prior \
     acceptances" =
  (* Two acceptors each report a different previously-accepted value.
     One accepted "round-3-value" at seq=3, another "round-7-value" at seq=7.
     The proposer must adopt the value from seq=7, the highest seen.
     This models the case where a previous leader got partway through
     two different rounds before crashing -- only the most recent matters. *)
  let lower = promise_with_value 3 "round-3-value" in
  let higher = promise_with_value 7 "round-7-value" in
  let rs =
    waiting_state ~promises:[ lower; higher ] 1 (make_vs "my-preferred-value")
  in
  (match NS.is_quorum_reached 3 rs with
  | NS.MajorityGrants a ->
      Stdio.printf "adopted: %s\n" (NS.V.to_string a.T.value)
  | _ -> Stdio.print_string "no quorum\n");
  [%expect {| adopted: round-7-value |}]

let%expect_test
    "value-adoption: proposer's own value wins when it has the highest \
     proposal_id" =
  (* A prior acceptance exists at seq=2, but our proposer is at seq=9.
     The proposer's own assertion has the highest id in the set, so it
     wins -- the prior accepted value is stale relative to our round.
     This is the case where a new leader supersedes all previous attempts.
     More relevant for extension implementations (v1) of this project though. *)
  let stale = promise_with_value 2 "stale-value" in
  let rs =
    waiting_state ~promises:[ stale; blank_promise ] 9
      (make_vs "fresh-leader-value")
  in
  (match NS.is_quorum_reached 3 rs with
  | NS.MajorityGrants a ->
      Stdio.printf "adopted: %s\n" (NS.V.to_string a.T.value)
  | _ -> Stdio.print_string "no quorum\n");
  [%expect {| adopted: fresh-leader-value |}]

let%expect_test
    "value-adoption: promise ordering is irrelevant -- highest proposal_id \
     always wins regardless of arrival order" =
  (* Defensive check: the fold must find the global maximum,
     not the first or last element. Promises arrive highest-first here.
     This guards against an implementation that short-circuits on the
     first non-blank promise rather than scanning all of them. *)
  let p_seq7 = promise_with_value 7 "seven" in
  let p_seq3 = promise_with_value 3 "three" in
  let p_seq5 = promise_with_value 5 "five" in
  let rs =
    waiting_state ~promises:[ p_seq7; p_seq3; p_seq5 ] 1
      (make_vs "proposer-value")
  in
  (match NS.is_quorum_reached 5 rs with
  | NS.MajorityGrants a ->
      Stdio.printf "adopted: %s\n" (NS.V.to_string a.T.value)
  | _ -> Stdio.print_string "no quorum\n");
  [%expect {| adopted: seven |}]

(* %%%%%%%%  [ Phase 2 acceptability, gate for phase 1 commitments ] %%%%%%%%%%%%%

    When an acceptor grants permission in phase 1, it makes a promise: "I will
    not accept any suggestion with a lower proposal_id than the one I just
    granted." Phase 2 is where that promise is honoured.

    The structural difference from is_proposal_permissible is intentional:
    is_suggestion_acceptable calls Option.value_exn on the current promise
    rather than defaulting to true. This encodes the protocol invariant that
    a suggestion can only legally arrive AFTER phase 1 has completed -- so an
    active promise is guaranteed to exist. Calling it on an idle acceptor is a
    protocol violation, not a valid input, and the implementation treats it
    as such.

    Three properties under test:

    1. A suggestion matching the promised proposal_id is accepted
    2. A suggestion with a higher proposal_id is also accepted (the >= check
    allows a proposer to re-use a granted slot)
    3. A suggestion from a stale/lower proposal_id is rejected -- the acceptor's
    earlier promise to a higher round holds firm.
*)

(** Build a role_state whose acceptor has granted permission for [seq] -- i.e.
    it is in the Accepting state with a current promise. This represents an
    acceptor that has completed phase 1. *)
let accepting_state_with_promise seq value : NS.role_state =
  let promised = assertion seq value in
  NS.init_state () |> NS.idle_of |> fun rs ->
  NS.set_role rs NS.Acceptor
    (NS.Accepting { promised = Some promised; accepted = None })

let%expect_test
    "phase2/acceptability: suggestion matching the promised proposal_id is \
     accepted" =
  (* The canonical case: the proposer sends a suggestion with the same
     proposal_id that the acceptor granted permission for in phase 1.
     The acceptor must honour its promise and accept. *)
  let rs = accepting_state_with_promise 5 (make_vs "agreed-value") in
  let result = NS.is_suggestion_acceptable rs (pid 5) in
  Stdio.print_string (if result then "accepted" else "rejected");
  [%expect {| accepted |}]

let%expect_test
    "phase2/acceptability: suggestion with a higher proposal_id than promised \
     is also accepted" =
  (* A proposer may legitimately arrive with a higher proposal_id than
     the one it originally requested permission for -- for example if it
     retried with a fresh id after a competing round. The >= check means
     the acceptor's promise covers this case: it committed to accepting
     anything at least as high as what it granted. *)
  let rs = accepting_state_with_promise 5 (make_vs "agreed-value") in
  let result = NS.is_suggestion_acceptable rs (pid 8) in
  Stdio.print_string (if result then "accepted" else "rejected");
  [%expect {| accepted |}]

let%expect_test
    "phase2/acceptability: suggestion from a stale lower proposal is rejected \
     -- the promise holds firm" =
  (* This is the safety-critical case. To give an example,
     an old leader that was delayed or partitioned may still send a
     suggestion for a proposal_id that the acceptor has since superseded by
     granting permission to a higher round. The acceptor MUST reject it.
     Accepting would risk deciding a value that a newer round has already
     overridden, violating consensus safety. *)
  let rs = accepting_state_with_promise 7 (make_vs "newer-round-value") in
  let result = NS.is_suggestion_acceptable rs (pid 3) in
  Stdio.print_string (if result then "accepted" else "rejected");
  [%expect {| rejected |}]

let%expect_test
    "phase2/acceptability: the promise boundary is exact -- one below the \
     promised id is rejected, the id itself is accepted" =
  (* Boundary test for the >= comparison. Proposal ids seq=4 and seq=5
     straddle the promise boundary at seq=5. This guards against an
     off-by-one in the comparison that would be invisible to the three
     cases above. *)
  let rs = accepting_state_with_promise 5 (make_vs "some-value") in
  let below = NS.is_suggestion_acceptable rs (pid 4) in
  let exact = NS.is_suggestion_acceptable rs (pid 5) in
  Stdio.printf "seq=4 (below promised): %s\n"
    (if below then "accepted" else "rejected");
  Stdio.printf "seq=5 (exact match):    %s\n"
    (if exact then "accepted" else "rejected");
  [%expect
    {|
    seq=4 (below promised): rejected
    seq=5 (exact match):    accepted
    |}]

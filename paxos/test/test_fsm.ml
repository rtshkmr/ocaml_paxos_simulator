(* [fsm] tests for the Paxos state-machine layer.

    These tests exercise the role sub-states, transitions, and invariant
    predicates in {!Node_state.Make_node_state} *without* spinning up a
    simulator or constructing full nodes.  They complement the quorum
    tests (which verify decision logic) and the e2e tests (which verify
    end-to-end protocol behaviour) by focusing on the shape and
    boundaries of the finite state machines themselves.

    A consensus protocol's safety rests on state-machine invariants:
    certain operations must only occur in certain states.  Some of
    those invariants are enforced at compile time via GADTs (the
    role_selector prevents "wrong role" mutations).  Others are still
    enforced at runtime via [failwith] guards (preventing "wrong state
    within a role" calls).

    This file serves three purposes:

    1. **Ledger**.  It catalogues every [failwith] / [assert false]
       guard in the FSM layer, verifies that each one fires on the
       expected illegal input, and documents *why* the guard exists.

    2. **Safety net**.  If a future refactor removes or weakens a guard,
       these tests break -- making the debt visible rather than silent.

    3. **Pedagogical artefact**.  The test names and section headers are
       structured so the blog post can reference them when it says
       "the failwith calls stay -- they're documented and tested."

    ---------------------------------------------------------------
    Catalogue of runtime guards in the FSM layer (node_state.ml):
    ---------------------------------------------------------------

    | Function                  | Guard      | Illegal input                                         | Tested below |
    |---------------------------+------------+-------------------------------------------------------+--------------|
    | is_quorum_reached         | failwith   | proposer in Idle, Preparing, Decided, ProposerInactive | yes          |
    | current_promise           | assert false | acceptor in AcceptorInactive                          | yes          |
    | last_accepted_promise     | assert false | acceptor in AcceptorInactive                          | yes          |

    Guards in the node-level dispatch layer (node.ml) -- tested
    implicitly by the e2e suite but not directly here, since they
    require a fully wired node + bus:

    | Function                         | Guard    | Illegal input                        |
    |----------------------------------+----------+--------------------------------------|
    | propose                          | failwith | proposer not Idle                    |
    | announce_decision                | failwith | proposer not Decided                 |
    | update_proposer_state_on_nack    | failwith | proposer not WaitingForPromises / PA |
    | handle_coordination (catch-all)  | failwith | non-coordination message variant     |
    | get_handler_for_topic (catch-all) | failwith | unsupported topic                   |
*)

open! Base
open Paxos
module VS = Value_string.Value_string
module NS = Node_state.Make_node_state (VS)
module T = Types.Types

(** Value constructor, same as test_quorum.ml -- works around the abstract
    [NS.V.t] by round-tripping through sexp. *)
let make_vs s = NS.V.t_of_sexp (Sexplib.Sexp.Atom s)

let pid ?(node = 1) seq = T.make_proposal_id ~seq ~node

let assertion ?(node = 1) seq value : NS.assertion =
  { T.proposal = pid ~node seq; value }

(** Renders a proposer_state to a short human-readable label. *)
let proposer_label = function
  | NS.ProposerInactive -> "ProposerInactive"
  | NS.Idle -> "Idle"
  | NS.Preparing _ -> "Preparing"
  | NS.WaitingForPromises _ -> "WaitingForPromises"
  | NS.ProposerAccepting _ -> "ProposerAccepting"
  | NS.Decided _ -> "Decided"

(** Renders an acceptor_state to a short human-readable label. *)
let acceptor_label = function
  | NS.AcceptorInactive -> "AcceptorInactive"
  | NS.Idle -> "Idle"
  | NS.Accepting _ -> "Accepting"

(** Renders a learner_state to a short human-readable label. *)
let learner_label = function
  | NS.Learned assertions ->
      Printf.sprintf "Learned(%d)" (List.length assertions)

(** Prints a one-line summary of all three role states. *)
let show_roles rs =
  Stdio.printf "proposer: %s | acceptor: %s | learner: %s\n"
    (NS.get_role rs NS.Proposer |> proposer_label)
    (NS.get_role rs NS.Acceptor |> acceptor_label)
    (NS.get_role rs NS.Learner |> learner_label)

(** Traps a [failwith] or [assert false] and prints the outcome. *)
let trap_exn f =
  try f () with
  | Failure msg -> Stdio.printf "failwith: %s\n" msg
  | _ -> Stdio.print_string "assert false (or other exn)\n"

(* %%%%%%%%  [ Initial state -- the blank slate ] %%%%%%%%%%%%%

    Every node begins life with all three roles in their idle positions:
    the proposer is Idle (ready to initiate), the acceptor is Idle (no
    prior promises), and the learner has an empty assertion list.

    This is the only state the system enters without going through
    transition_role_state -- it is the state we start reasoning from.
*)

let%expect_test "init: fresh role_state has all roles idle" =
  let rs = NS.init_state () in
  show_roles rs;
  [%expect {| proposer: Idle | acceptor: Idle | learner: Learned(0) |}]

let%expect_test "init: fresh role_state is not inactive" =
  let rs = NS.init_state () in
  Stdio.printf "is_inactive: %b\n" (NS.is_inactive rs);
  [%expect {| is_inactive: false |}]

(* %%%%%%%%  [ GADT role_selector -- the compile-time guarantee ] %%%%%%%%%%%%%

    The role_selector GADT solves the "wrong role" class of bugs.
    Each constructor (Proposer, Acceptor, Learner) indexes its type
    parameter to the corresponding sub-state, so passing a
    proposer_state where the selector says Acceptor is a *type error*
    caught at compile time -- it never reaches runtime.

    The tests below verify the dynamic behaviour of get_role / set_role.
    The compile-time guarantee is not testable per se (it's a type error,
    not a runtime failure), but we verify that the GADT dispatch reaches
    the right field and that cross-role independence holds: mutating the
    proposer state does not disturb the acceptor or learner.
*)

let%expect_test "gadt/get_role: each selector reads the correct sub-state" =
  let a = assertion 1 (make_vs "val") in
  let rs =
    NS.init_state () |> fun rs -> NS.set_role rs NS.Proposer (NS.Preparing a)
  in
  Stdio.printf "proposer: %s\n" (NS.get_role rs NS.Proposer |> proposer_label);
  Stdio.printf "acceptor: %s\n" (NS.get_role rs NS.Acceptor |> acceptor_label);
  Stdio.printf "learner: %s\n" (NS.get_role rs NS.Learner |> learner_label);
  [%expect
    {|
    proposer: Preparing
    acceptor: Idle
    learner: Learned(0)
    |}]

let%expect_test
    "gadt/set_role: proposer mutation does not disturb acceptor or learner" =
  (* Start with a non-trivial state across all three roles, then
     mutate only the proposer.  Acceptor and learner must survive
     unchanged.  This confirms set_role's functional update semantics
     and the GADT's role isolation. *)
  let a = assertion 1 (make_vs "x") in
  let base =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Acceptor
      (NS.Accepting { promised = Some a; accepted = None })
    |> fun rs -> NS.set_role rs NS.Learner (NS.Learned [ a ])
  in
  let updated = NS.set_role base NS.Proposer (NS.Decided (make_vs "done")) in
  Stdio.printf "before: ";
  show_roles base;
  Stdio.printf "after:  ";
  show_roles updated;
  [%expect
    {|
    before: proposer: Idle | acceptor: Accepting | learner: Learned(1)
    after:  proposer: Decided | acceptor: Accepting | learner: Learned(1)
    |}]

let%expect_test
    "gadt/set_role: acceptor mutation does not disturb proposer or learner" =
  let a = assertion 3 (make_vs "y") in
  let base =
    NS.init_state () |> fun rs -> NS.set_role rs NS.Proposer (NS.Preparing a)
  in
  let updated =
    NS.set_role base NS.Acceptor
      (NS.Accepting { promised = Some a; accepted = None })
  in
  Stdio.printf "before: ";
  show_roles base;
  Stdio.printf "after:  ";
  show_roles updated;
  [%expect
    {|
    before: proposer: Preparing | acceptor: Idle | learner: Learned(0)
    after:  proposer: Preparing | acceptor: Accepting | learner: Learned(0)
    |}]

(* %%%%%%%%  [ Legal proposer state flow -- the FSM happy path ] %%%%%%%%%%%%%

    The proposer FSM has the richest state space: six constructors,
    two quorum-decision gates, and a simulation-control state.  The
    tests below walk through the full legal transition sequence:

        Idle → Preparing → WaitingForPromises → ProposerAccepting → Decided

    Each step constructs the target state by hand (we are testing the
    state representation, not the message-handling logic) and verifies
    that the role_state reflects the transition.

    The key insight: every one of these transitions would go through
    transition_role_state in production, which is the single mutation
    chokepoint enforced by the GADT.  These tests verify that the
    states themselves are well-formed and distinguishable.
*)

let%expect_test "fsm/proposer: Idle → Preparing" =
  let a = assertion 1 (make_vs "riot") in
  let rs =
    NS.init_state () |> fun rs -> NS.set_role rs NS.Proposer (NS.Preparing a)
  in
  Stdio.printf "proposer: %s\n" (NS.get_role rs NS.Proposer |> proposer_label);
  [%expect {| proposer: Preparing |}]

let%expect_test "fsm/proposer: Preparing → WaitingForPromises" =
  let a = assertion 1 (make_vs "riot") in
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer
      (NS.WaitingForPromises
         { assertion = a; promises_received = []; nacks_received = [] })
  in
  Stdio.printf "proposer: %s\n" (NS.get_role rs NS.Proposer |> proposer_label);
  [%expect {| proposer: WaitingForPromises |}]

let%expect_test "fsm/proposer: WaitingForPromises → ProposerAccepting" =
  let a = assertion 1 (make_vs "riot") in
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer
      (NS.ProposerAccepting { assertion = a; acks = []; nacks_received = [] })
  in
  Stdio.printf "proposer: %s\n" (NS.get_role rs NS.Proposer |> proposer_label);
  [%expect {| proposer: ProposerAccepting |}]

let%expect_test "fsm/proposer: ProposerAccepting → Decided" =
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer (NS.Decided (make_vs "consensus achieved"))
  in
  Stdio.printf "proposer: %s\n" (NS.get_role rs NS.Proposer |> proposer_label);
  [%expect {| proposer: Decided |}]

let%expect_test
    "fsm/proposer: full journey from Idle to Decided preserves other roles" =
  (* Walk through all four transitions in sequence, checking that the
     acceptor and learner are undisturbed at each step. *)
  let a = assertion 1 (make_vs "full-journey") in
  let step_1 = NS.set_role (NS.init_state ()) NS.Proposer (NS.Preparing a) in
  let step_2 =
    NS.set_role step_1 NS.Proposer
      (NS.WaitingForPromises
         { assertion = a; promises_received = []; nacks_received = [] })
  in
  let step_3 =
    NS.set_role step_2 NS.Proposer
      (NS.ProposerAccepting
         { assertion = a; acks = [ 2; 3 ]; nacks_received = [] })
  in
  let step_4 =
    NS.set_role step_3 NS.Proposer (NS.Decided (make_vs "full-journey"))
  in
  List.iter
    [
      ("init", NS.init_state ());
      ("preparing", step_1);
      ("waiting", step_2);
      ("accepting", step_3);
      ("decided", step_4);
    ]
    ~f:(fun (label, rs) ->
      Stdio.printf "%-10s → " label;
      show_roles rs);
  [%expect
    {|
    init       → proposer: Idle | acceptor: Idle | learner: Learned(0)
    preparing  → proposer: Preparing | acceptor: Idle | learner: Learned(0)
    waiting    → proposer: WaitingForPromises | acceptor: Idle | learner: Learned(0)
    accepting  → proposer: ProposerAccepting | acceptor: Idle | learner: Learned(0)
    decided    → proposer: Decided | acceptor: Idle | learner: Learned(0)
    |}]

(* %%%%%%%%  [ Acceptor state flow ] %%%%%%%%%%%%%

    The acceptor FSM is simpler: three states, with Idle and Accepting
    being the protocol-relevant pair.  The tests verify that the
    acceptor can transition from Idle to Accepting (when granting
    permission), and that the Accepting record correctly tracks
    promised and accepted values.
*)

let%expect_test "fsm/acceptor: Idle → Accepting (permission granted)" =
  let a = assertion 5 (make_vs "promised-value") in
  let rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      (NS.Accepting { promised = Some a; accepted = None })
  in
  Stdio.printf "acceptor: %s\n" (NS.get_role rs NS.Acceptor |> acceptor_label);
  Stdio.printf "has promise: %b\n" (NS.current_promise rs |> Option.is_some);
  [%expect {|
    acceptor: Accepting
    has promise: true
    |}]

let%expect_test "fsm/acceptor: Accepting tracks both promised and accepted" =
  let promised = assertion 5 (make_vs "v1") in
  let accepted = assertion 5 (make_vs "v1") in
  let rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      (NS.Accepting { promised = Some promised; accepted = Some accepted })
  in
  Stdio.printf "current_promise is_some: %b\n"
    (NS.current_promise rs |> Option.is_some);
  Stdio.printf "last_accepted is_some: %b\n"
    (NS.last_accepted_promise rs |> Option.is_some);
  [%expect
    {|
    current_promise is_some: true
    last_accepted is_some: true
    |}]

(* %%%%%%%%  [ Learner state flow ] %%%%%%%%%%%%%

    The learner FSM is trivial -- a single constructor that accumulates
    assertions.  But it is worth testing because the accumulation order
    matters: new assertions are prepended, and the most recent one is
    the one the test harness inspects when checking safety.
*)

let%expect_test "fsm/learner: assertions accumulate in prepend order" =
  let a1 = assertion 1 (make_vs "first") in
  let a2 = assertion 2 (make_vs "second") in
  let rs_0 = NS.init_state () in
  let rs_1 = NS.set_role rs_0 NS.Learner (NS.Learned [ a1 ]) in
  let rs_2 = NS.set_role rs_1 NS.Learner (NS.Learned [ a2; a1 ]) in
  Stdio.printf "step 0: %s\n" (NS.get_role rs_0 NS.Learner |> learner_label);
  Stdio.printf "step 1: %s\n" (NS.get_role rs_1 NS.Learner |> learner_label);
  Stdio.printf "step 2: %s\n" (NS.get_role rs_2 NS.Learner |> learner_label);
  [%expect
    {|
    step 0: Learned(0)
    step 1: Learned(1)
    step 2: Learned(2)
    |}]

(* %%%%%%%%  [ Simulation vs protocol states -- root of the failwith problem ] %%%

    The simulation layer adds two states -- ProposerInactive and
    AcceptorInactive -- to the same flat variants that carry the
    protocol-level states.  This is the design decision that forces
    the failwith guards: the type system sees all six proposer states
    as siblings, so it cannot distinguish "active protocol state" from
    "simulation inactive state" at compile time.

    The tests below verify that the simulation-level transitions
    (idle_of, inactive_of) behave correctly, and that is_inactive
    detects both inactive constructors.
*)

let%expect_test
    "sim-states/inactive_of: sets both proposer and acceptor to inactive" =
  let rs = NS.init_state () |> NS.inactive_of in
  show_roles rs;
  Stdio.printf "is_inactive: %b\n" (NS.is_inactive rs);
  [%expect
    {|
    proposer: ProposerInactive | acceptor: AcceptorInactive | learner: Learned(0)
    is_inactive: true
    |}]

let%expect_test
    "sim-states/idle_of: restores both proposer and acceptor to Idle" =
  let rs = NS.init_state () |> NS.inactive_of |> NS.idle_of in
  show_roles rs;
  Stdio.printf "is_inactive: %b\n" (NS.is_inactive rs);
  [%expect
    {|
    proposer: Idle | acceptor: Idle | learner: Learned(0)
    is_inactive: false
    |}]

let%expect_test
    "sim-states/idle_of: preserves learner state across activation cycle" =
  (* A node that learned something before being deactivated should
     retain its learned assertions when reactivated.  idle_of only
     touches proposer and acceptor. *)
  let a = assertion 1 (make_vs "remembered") in
  let rs =
    NS.init_state () |> fun rs -> NS.set_role rs NS.Learner (NS.Learned [ a ])
  in
  let after_cycle = rs |> NS.inactive_of |> NS.idle_of in
  Stdio.printf "learner after cycle: %s\n"
    (NS.get_role after_cycle NS.Learner |> learner_label);
  [%expect {| learner after cycle: Learned(1) |}]

let%expect_test
    "sim-states/is_inactive: triggers on either ProposerInactive or \
     AcceptorInactive" =
  (* is_inactive checks both roles with an OR -- if *either* role is
     in its inactive variant, the node is considered inactive.  This
     means a partially-inactive state (one role inactive, other idle)
     also registers as inactive. *)
  let only_proposer_inactive =
    NS.set_role (NS.init_state ()) NS.Proposer NS.ProposerInactive
  in
  let only_acceptor_inactive =
    NS.set_role (NS.init_state ()) NS.Acceptor NS.AcceptorInactive
  in
  Stdio.printf "proposer inactive only: %b\n"
    (NS.is_inactive only_proposer_inactive);
  Stdio.printf "acceptor inactive only: %b\n"
    (NS.is_inactive only_acceptor_inactive);
  [%expect
    {|
    proposer inactive only: true
    acceptor inactive only: true
    |}]

(* %%%%%%%%  [ The failwith boundary -- is_quorum_reached ] %%%%%%%%%%%%%

    is_quorum_reached is the most important runtime guard in the FSM
    layer.  It is the function the blog post discusses when it says
    "every failwith in a pattern match is a statement that the
    programmer knows something the compiler doesn't."

    The function is only valid when the proposer is in one of two
    states: WaitingForPromises (phase 1 quorum) or ProposerAccepting
    (phase 2 quorum).  All other proposer states are *illegal inputs*.
    The implementation guards against them with a wildcard + failwith.

    The tests below verify that:
    1. Legal inputs (WaitingForPromises, ProposerAccepting) return
       quorum results without raising -- covered by test_quorum.ml
       but confirmed here for completeness.
    2. Illegal inputs (Idle, Preparing, Decided, ProposerInactive)
       raise a Failure exception.

    Each illegal-input test is a *record of debt*: it documents a
    branch that the type system cannot currently prevent.  If the
    two-layer GADT refactor is ever completed, these tests should
    become *compilation errors* rather than runtime traps.
*)

let%expect_test
    "failwith/is_quorum_reached: WaitingForPromises is legal (no exception)" =
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer
      (NS.WaitingForPromises
         {
           assertion = assertion 1 (make_vs "v");
           promises_received = [];
           nacks_received = [];
         })
  in
  trap_exn (fun () ->
      let _ = NS.is_quorum_reached 3 rs in
      Stdio.print_string "ok: returned quorum result");
  [%expect {| ok: returned quorum result |}]

let%expect_test
    "failwith/is_quorum_reached: ProposerAccepting is legal (no exception)" =
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer
      (NS.ProposerAccepting
         {
           assertion = assertion 1 (make_vs "v");
           acks = [];
           nacks_received = [];
         })
  in
  trap_exn (fun () ->
      let _ = NS.is_quorum_reached 3 rs in
      Stdio.print_string "ok: returned quorum result");
  [%expect {| ok: returned quorum result |}]

let%expect_test
    "failwith/is_quorum_reached: Idle is ILLEGAL -- the programmer's pinky \
     promise" =
  (* An Idle proposer has not initiated any round.  Asking whether it
     reached quorum is a protocol violation.  The failwith fires. *)
  let rs = NS.init_state () in
  trap_exn (fun () ->
      let _ = NS.is_quorum_reached 3 rs in
      ());
  [%expect
    {| failwith: is_quorum_reached called on non-proposer state (must be WaitingForPromises or ProposerAccepting) |}]

let%expect_test
    "failwith/is_quorum_reached: Preparing is ILLEGAL -- round not yet \
     broadcast" =
  (* Preparing means the proposal has been constructed but the
     permission request hasn't been sent yet.  No promises could have
     arrived.  Quorum is meaningless here. *)
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer
      (NS.Preparing (assertion 1 (make_vs "too-early")))
  in
  trap_exn (fun () ->
      let _ = NS.is_quorum_reached 3 rs in
      ());
  [%expect
    {| failwith: is_quorum_reached called on non-proposer state (must be WaitingForPromises or ProposerAccepting) |}]

let%expect_test
    "failwith/is_quorum_reached: Decided is ILLEGAL -- consensus already \
     reached" =
  (* A Decided proposer has already chosen a value.  Rechecking
     quorum is nonsensical.  This guard prevents stale messages
     from re-triggering decision logic. *)
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer (NS.Decided (make_vs "already-done"))
  in
  trap_exn (fun () ->
      let _ = NS.is_quorum_reached 3 rs in
      ());
  [%expect
    {| failwith: is_quorum_reached called on non-proposer state (must be WaitingForPromises or ProposerAccepting) |}]

let%expect_test
    "failwith/is_quorum_reached: ProposerInactive is ILLEGAL -- the \
     simulation-level intruder" =
  (* This is the case that motivates the entire GADT discussion.
     ProposerInactive is a *simulation-level* state that lives in the
     same sum type as the protocol states.  The type system sees it
     as a valid sibling of WaitingForPromises.  The failwith is the
     only thing stopping it from reaching quorum logic.

     In the two-layer GADT plan, this would be a type error instead. *)
  let rs = NS.init_state () |> NS.inactive_of in
  trap_exn (fun () ->
      let _ = NS.is_quorum_reached 3 rs in
      ());
  [%expect
    {| failwith: is_quorum_reached called on non-proposer state (must be WaitingForPromises or ProposerAccepting) |}]

(* %%%%%%%%  [ The assert-false boundary -- acceptor promise accessors ] %%%%%

    current_promise and last_accepted_promise extract the acceptor's
    promise/acceptance records.  They handle two legal states:
    - Idle → returns None (no prior record)
    - Accepting → returns the stored promise

    The wildcard arm catches AcceptorInactive with [assert false].
    This is even more aggressive than failwith: it signals "this is
    not merely unexpected -- it is a programming error that should
    never compile in an ideal world."

    These guards exist because AcceptorInactive lives in the same
    acceptor_state variant as Idle and Accepting.  The root cause
    is identical to the proposer's failwith problem.
*)

let%expect_test
    "assert-false/current_promise: Idle acceptor returns None (legal)" =
  let rs = NS.init_state () in
  Stdio.printf "current_promise: %s\n"
    (match NS.current_promise rs with None -> "None" | Some _ -> "Some");
  [%expect {| current_promise: None |}]

let%expect_test
    "assert-false/current_promise: Accepting acceptor returns the promise \
     (legal)" =
  let a = assertion 5 (make_vs "promised") in
  let rs =
    NS.init_state () |> NS.idle_of |> fun rs ->
    NS.set_role rs NS.Acceptor
      (NS.Accepting { promised = Some a; accepted = None })
  in
  Stdio.printf "current_promise: %s\n"
    (match NS.current_promise rs with None -> "None" | Some _ -> "Some");
  [%expect {| current_promise: Some |}]

let%expect_test
    "assert-false/current_promise: AcceptorInactive triggers assert false" =
  let rs = NS.init_state () |> NS.inactive_of in
  trap_exn (fun () ->
      let _ = NS.current_promise rs in
      ());
  [%expect {| assert false (or other exn) |}]

let%expect_test
    "assert-false/last_accepted_promise: AcceptorInactive triggers assert false"
    =
  let rs = NS.init_state () |> NS.inactive_of in
  trap_exn (fun () ->
      let _ = NS.last_accepted_promise rs in
      ());
  [%expect {| assert false (or other exn) |}]

(* %%%%%%%%  [ Interaction: quorum + FSM state coherence ] %%%%%%%%%%%%%

    The final section ties the FSM structure back to the quorum logic.
    A correctly-wired protocol only calls is_quorum_reached after the
    proposer has accumulated some responses.  The tests below verify
    that quorum results are coherent with the proposer state they
    emerge from -- i.e. a WaitingForPromises state with no promises
    yields NotReached, and the same state with enough promises yields
    MajorityGrants.

    These are a smoke-check that the FSM's state representation
    carries enough information for the quorum predicates to work.
    The exhaustive quorum arithmetic is in test_quorum.ml.
*)

let%expect_test "coherence: empty WaitingForPromises yields NotReached" =
  let a = assertion 1 (make_vs "hope") in
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer
      (NS.WaitingForPromises
         { assertion = a; promises_received = []; nacks_received = [] })
  in
  (match NS.is_quorum_reached 3 rs with
  | NS.NotReached -> Stdio.print_string "NotReached"
  | NS.MajorityGrants _ -> Stdio.print_string "MajorityGrants"
  | NS.MajorityNacks _ -> Stdio.print_string "MajorityNacks");
  [%expect {| NotReached |}]

let%expect_test
    "coherence: WaitingForPromises with majority blank promises yields \
     MajorityGrants" =
  let a = assertion 1 (make_vs "hope") in
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer
      (NS.WaitingForPromises
         {
           assertion = a;
           promises_received = [ None; None ];
           nacks_received = [];
         })
  in
  (match NS.is_quorum_reached 3 rs with
  | NS.NotReached -> Stdio.print_string "NotReached"
  | NS.MajorityGrants ga ->
      Stdio.printf "MajorityGrants(%s)\n" (ga.value |> VS.to_string)
  | NS.MajorityNacks _ -> Stdio.print_string "MajorityNacks");
  [%expect {| MajorityGrants(hope) |}]

let%expect_test
    "coherence: ProposerAccepting with majority acks yields MajorityGrants" =
  let a = assertion 1 (make_vs "agreement") in
  let rs =
    NS.init_state () |> fun rs ->
    NS.set_role rs NS.Proposer
      (NS.ProposerAccepting
         { assertion = a; acks = [ 2; 3 ]; nacks_received = [] })
  in
  (match NS.is_quorum_reached 3 rs with
  | NS.NotReached -> Stdio.print_string "NotReached"
  | NS.MajorityGrants ga ->
      Stdio.printf "MajorityGrants(%s)\n" (ga.value |> VS.to_string)
  | NS.MajorityNacks _ -> Stdio.print_string "MajorityNacks");
  [%expect {| MajorityGrants(agreement) |}]

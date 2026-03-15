(* [e2e] + [integration] scenario tests for the Paxos simulator.

    These tests spin up a headless {!Simulator.Simulator}, seed nodes, inject
    events, run to completion, and then assert on the resulting node states.

    Because they exercise the full message dispatch loop, they are the right
    place to verify cross-cutting properties like:
    - Safety: no two nodes ever decide differently
    - Liveness (conditional): if a majority can communicate, they decide
*)

open! Base
open Paxos
module Sim = Simulator.Simulator

(* %%%%%%%%  [ Integration: harness sanity ] %%%%%%%%%%%%%

    Before testing protocol behaviour, we confirm that the simulation
    infrastructure itself is sound. These tests exercise nothing about
    Paxos -- they exist to ensure that the test harness can construct a
    simulator, seed nodes, and run to completion without crashing.

    A failure here means something is wrong with the test setup, not
    with the protocol implementation. All subsequent tests depend on
    this machinery working correctly.
*)

let%expect_test
    "integration: simulator can be constructed and stepped without crashing" =
  let sim = Test_utils.make_silent_sim ~max_ticks:3 () in
  (* Run 3 ticks with no nodes; just confirm the machinery doesn't explode. *)
  Test_utils.run_to_completion sim;
  Stdio.print_string "it doesn't blow up in our face";
  [%expect {| it doesn't blow up in our face |}]

let%expect_test
    "integration: 3 nodes can be seeded and their aliases are visible" =
  let sim = Test_utils.make_silent_sim ~max_ticks:1 () in
  Test_utils.seed_n_nodes sim 3 ~cluster_size:3;
  Sim.get_nodes sim |> List.map ~f:Sim.N.alias_of
  |> List.sort ~compare:String.compare
  |> List.iter ~f:(Stdio.printf "%s\n");
  [%expect {|
    node_1
    node_2
    node_3
    |}]

(* %%%%%%%%  [ E2E: happy path -- consensus in a healthy cluster ] %%%%%%%%%%%%%

    The baseline scenario: all nodes are alive, one node proposes a value,
    and the protocol runs to completion without any faults.

    In single-decree Paxos the roles are asymmetric. Only the proposer
    transitions to Decided -- it is the node that drove the round and knows
    with certainty that its value was chosen. All other nodes, including the
    proposer itself in its learner role, transition to Learned once they
    receive the decided broadcast.

    Expected outcome: 1 Decided, 3 Learned. Every learned node carries the
    same value. This is the safety property in its simplest form.
*)

let%expect_test
    "e2e/happy-path: 3 nodes, 1 proposer, all learn the same value, initiator \
     decides" =
  let sim = Test_utils.make_silent_sim ~max_ticks:30 () in
  Test_utils.seed_n_nodes sim 3 ~cluster_size:3;
  Test_utils.schedule_proposal sim ~at:1 ~alias:"node_1" ~value:"time to riot"
    ();
  Test_utils.run_to_completion sim;
  Stdio.printf "Decided: %d/3 | Learned: %d/3\n"
    (Test_utils.count_decided sim)
    (Test_utils.count_learned sim);
  Test_utils.print_decision_summary sim;
  Test_utils.print_learned_summary sim;
  (match Test_utils.all_learned sim with
  | None -> Stdio.print_string "safety: no learning yet\n"
  | Some (value, true) -> Stdio.printf "safety: all learned '%s' ✓\n" value
  | Some (_, false) -> Stdio.print_string "safety: LEARNING DISAGREEMENT! ✗\n");
  [%expect
    {|
    Decided: 1/3 | Learned: 3/3
    node_1: Decided
    node_2: Pending
    node_3: Pending
    node_1: Learned
    node_2: Learned
    node_3: Learned
    safety: all learned 'time to riot' ✓
    |}]

(* %%%%%%%%  [ E2E: quorum failure -- liveness requires a majority ] %%%%%%%%%%%%%

    Paxos guarantees safety unconditionally but only guarantees liveness
    when a majority of nodes can communicate. This test verifies the
    liveness boundary: when fewer than a majority of acceptors are
    available, no decision is possible and the proposer stalls.

    Two of three nodes are crashed before the proposal. The proposer sends
    permission requests into a void -- it can never accumulate the two
    promises it needs to proceed to phase 2. The cluster makes no progress.

    This is not a bug. It is the correct behaviour. A system that decided
    with only one node available would not be providing consensus.
*)

let%expect_test
    "e2e/quorum-failure: 2 nodes inactive, proposer cannot reach quorum" =
  let sim = Test_utils.make_silent_sim ~max_ticks:30 () in
  Test_utils.seed_n_nodes sim 3 ~cluster_size:3;
  Test_utils.schedule_deactivate sim ~at:1 ~alias:"node_2" ();
  Test_utils.schedule_deactivate sim ~at:1 ~alias:"node_3" ();
  Test_utils.schedule_proposal sim ~at:3 ~alias:"node_1"
    ~value:"we riot tonight" ();
  Test_utils.run_to_completion sim;
  Stdio.printf "decided: %d/%d\n" (Test_utils.count_decided sim) 3;
  Test_utils.print_decision_summary sim;
  [%expect
    {|
    decided: 0/3
    node_1: Pending
    node_2: Pending
    node_3: Pending
    |}]

(* %%%%%%%%  [ E2E: partition -- safety holds across a network split ] %%%%%%%%%%%%%

    A network partition splits the cluster into two islands that cannot
    communicate. The majority partition (2 of 3 nodes) retains liveness
    and can reach consensus. The minority partition (1 node) cannot form
    a quorum and makes no progress.

    The key safety property: if the minority node were somehow to decide,
    it would have to agree with the majority. In this scenario it never
    decides at all -- it simply does not receive the messages needed to
    participate.

    Expected outcome: the majority side reaches consensus (1 Decided,
    2 Learned). The isolated node stays pending on both counts.
*)

let%expect_test
    "e2e/partition: majority side learns, initiator decides, minority stays \
     pending" =
  (* 3-node cluster split 2+1.
     - node_1 and node_2 remain in partition 1 (the majority)
     - node_3 is moved to partition 2 at tick 1 (before proposal)
     - node_1 proposes at tick 3
     Expected: node_1 decides, node_1 and node_2 learn, node_3 stays pending.
     Safety: the two majority nodes must learn the same value. *)
  let sim = Test_utils.make_silent_sim ~max_ticks:40 () in
  Test_utils.seed_n_nodes sim 3 ~cluster_size:3;
  Test_utils.schedule_partition sim ~at:1 ~alias:"node_3" ~dest:2 ();
  Test_utils.schedule_proposal sim ~at:3 ~alias:"node_1"
    ~value:"we riot tonight" ();
  Test_utils.run_to_completion sim;
  Stdio.printf "Decided: %d/3 | Learned: %d/3\n"
    (Test_utils.count_decided sim)
    (Test_utils.count_learned sim);
  Test_utils.print_decision_summary sim;
  Test_utils.print_learned_summary sim;
  (match Test_utils.all_learned sim with
  | None -> Stdio.print_string "safety: no learning yet\n"
  | Some (value, true) -> Stdio.printf "safety: majority learned '%s' ✓\n" value
  | Some (_, false) -> Stdio.print_string "safety: LEARNING DISAGREEMENT! ✗\n");
  [%expect
    {|
    Decided: 1/3 | Learned: 2/3
    node_1: Decided
    node_2: Pending
    node_3: Pending
    node_1: Learned
    node_2: Learned
    node_3: Pending
    safety: majority learned 'we riot tonight' ✓
    |}]

(* %%%%%%%%  [ E2E: crash + recovery -- a failed round followed by consensus ] %%%%

    The most realistic failure scenario: a round fails because too many
    nodes crash before it can complete, then the cluster partially recovers
    and a new proposer drives a second round to completion.

    The sequence number increment on the second proposal is not incidental.
    node_1's acceptor may have recorded a promise from the first round.
    node_2 must use a strictly higher sequence number to be granted
    permission -- this is the proposal ordering invariant in practice,
    connecting the unit-level permissibility tests to observable
    end-to-end behaviour.

    Expected outcome: node_2 (the recovery initiator) decides. node_1
    and node_2 both learn the recovery value. node_3, which never came
    back online, misses the decision entirely.
*)

let%expect_test
    "e2e/crash-recovery: quorum fails, node_2 recovers and drives consensus" =
  (* Phase 1 -- quorum failure:
       node_2 and node_3 are deactivated before node_1 proposes.
       node_1 cannot gather a majority -- nothing is decided or learned.

     Phase 2 -- recovery:
       node_2 comes back online at tick 15.
       node_2 now proposes at tick 18 -- it is the new initiator.
       node_1 and node_2 form a majority (2 of 3), consensus is reached.
       node_3 stays inactive throughout and misses the decision. *)
  let sim = Test_utils.make_silent_sim ~max_ticks:50 () in
  Test_utils.seed_n_nodes sim 3 ~cluster_size:3;
  Test_utils.schedule_deactivate sim ~at:1 ~alias:"node_2" ();
  Test_utils.schedule_deactivate sim ~at:1 ~alias:"node_3" ();
  Test_utils.schedule_proposal sim ~at:3 ~alias:"node_1" ~value:"first try" ();
  Test_utils.schedule_activate sim ~at:15 ~alias:"node_2" ();
  Test_utils.schedule_proposal sim ~at:18 ~alias:"node_2"
    ~value:"node_2 takes over" ~seq:2 ();
  Test_utils.run_to_completion sim;
  Stdio.printf "Decided: %d/3 | Learned: %d/3\n"
    (Test_utils.count_decided sim)
    (Test_utils.count_learned sim);
  Test_utils.print_decision_summary sim;
  Test_utils.print_learned_summary sim;
  (match Test_utils.all_learned sim with
  | None -> Stdio.print_string "safety: no learning yet\n"
  | Some (value, true) ->
      Stdio.printf "safety: recovered cluster learned '%s' ✓\n" value
  | Some (_, false) -> Stdio.print_string "safety: LEARNING DISAGREEMENT! ✗\n");
  [%expect
    {|
    Decided: 1/3 | Learned: 2/3
    node_1: Pending
    node_2: Decided
    node_3: Pending
    node_1: Learned
    node_2: Learned
    node_3: Pending
    safety: recovered cluster learned 'node_2 takes over' ✓
    |}]

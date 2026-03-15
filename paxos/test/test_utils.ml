(* Exposes some shared test utilities for the Paxos simulator expect tests.

    Helps with:
    1. simulator lifecycle management:
       - building headless simulator instances --
    2. seeding nodes via spec structs
    3. scheduling proposals and control events
    4. inspecting node state after a run

    It's likely more beneficial for e2e test runs more than unit tests.
*)

open Base
open Paxos
module Sim = Simulator.Simulator
module Ev = Sim_event.Sim_event

(** Creates a headless simulator capped at [max_ticks].
    [allow_step] is NOT used here -- we drive the loop ourselves.
    Defaults to 30 ticks, which is enough for the basic happy-path scenario. *)
let make_sim ?(max_ticks = 30) ?(log_level = "DEBUG") () =
  Sim.of_spec {max_ticks= Some max_ticks; log_level}

let make_silent_sim ?(max_ticks = 30) () =
  make_sim ~max_ticks ~log_level:"OFF" ()

(** Steps the simulator until [is_runnable] returns false (i.e. [ max_ticks ]
    reached or the sim is halted). *)
let run_to_completion sim =
  let rec loop () = if Sim.is_runnable sim then (Sim.step sim ; loop ()) in
  loop ()

(** Builds a {!Sim.N.spec} for a single node.
    [node_id] is 0 by default because the simulator will overwrite it with
    the next counter value when hydrating. *)
let node_spec ?(cluster_size = 3) alias =
  { Sim.N.node_id= 0
  ; node_alias= alias
  ; topic_strs= ["Coordination"; "Simulation_control"; "Time"]
  ; initial_cluster_size= cluster_size
  ; initial_state= None
  ; storage_config= None }

(** Seeds [n] nodes named ["node_0"] .. ["node_{n-1}"] into [sim].
    All nodes share the same [cluster_size] because we're sticking to static
    node clusters without needing to handle presence checking.*)
let seed_n_nodes sim n ~cluster_size =
  let specs =
    List.init n ~f:(fun i ->
        node_spec ~cluster_size (Printf.sprintf "node_%d" i) )
  in
  Sim.seed_nodes_from_specs sim specs

(** Schedules a proposal event at logical time [at].
    [alias] is the proposer node's alias, [value] is the string value to
    propose, and [seq] is the proposal sequence number (defaults to 1). *)
let schedule_proposal sim ~at ~alias ~value ?(seq = 1) () =
  Sim.seed_events_from_specs sim
    [ { Ev.id= None
      ; time= at
      ; kind= "proposal"
      ; target= Some alias
      ; data= Some value
      ; args= Some [Int.to_string seq] } ]

(** Schedules a control event that moves [alias] to partition [dest] at
    logical time [at].  [dest] must be a partition id (integer ≥ 1). *)
let schedule_partition sim ~at ~alias ~dest () =
  Sim.seed_events_from_specs sim
    [ { Ev.id= None
      ; time= at
      ; kind= "control"
      ; target= Some alias
      ; data= Some "partition"
      ; args= Some [Int.to_string dest] } ]

(** Schedules a node deactivation ("crash") at logical time [at].
    This makes a node inactive. *)
let schedule_deactivate sim ~at ~alias () =
  Sim.seed_events_from_specs sim
    [ { Ev.id= None
      ; time= at
      ; kind= "control"
      ; target= Some alias
      ; data= Some "deactivate"
      ; args= None } ]

(** Returns a list of [(alias, state_sexp_string)] pairs for every node,
    sorted by alias for stable output in expect tests. *)
let node_states sim =
  Sim.get_nodes sim
  |> List.map ~f:(fun n -> (Sim.N.alias_of n, Sim.N.dump_state n))
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)

(** Returns [true] if the sexp state string contains the "Decided" constructor. *)
let is_decided_state s = String.is_substring s ~substring:"Decided"

(** Returns only the [(alias, state_str)] pairs for nodes that have decided. *)
let decided_nodes sim =
  node_states sim |> List.filter ~f:(fun (_, s) -> is_decided_state s)

(** Counts how many nodes have entered the [Decided] state. *)
let count_decided sim = decided_nodes sim |> List.length

(** Checks that every node in the cluster agrees on the same decided value.

    Returns:
    - [None]        if no node has decided yet
    - [Some (value, true)]  if all decided nodes agree
    - [Some (value, false)] if there is a disagreement (safety violation!) *)
let all_agree sim =
  match decided_nodes sim with
  | [] ->
      None
  | (_, first) :: rest ->
      let all_same =
        List.for_all rest ~f:(fun (_, s) -> String.equal s first)
      in
      Some (first, all_same)

(** Pretty-prints the decided state of every node for use in [%expect] blocks.
    Format: one line per node: "<alias>: Decided / <alias>: Pending" *)
let print_decision_summary sim =
  node_states sim
  |> List.iter ~f:(fun (alias, state) ->
      let label = if is_decided_state state then "Decided" else "Pending" in
      Stdio.printf "%s: %s\n" alias label )

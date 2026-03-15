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
        node_spec ~cluster_size (Printf.sprintf "node_%d" (i + 1)) )
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

(** Schedules a node reactivation ("revive") at logical time [at].
    This makes a node inactive. *)
let schedule_activate sim ~at ~alias () =
  Sim.seed_events_from_specs sim
    [ { Ev.id= None
      ; time= at
      ; kind= "control"
      ; target= Some alias
      ; data= Some "activate"
      ; args= None } ]

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

(** Returns a list of [(alias, role_state_sexp : Sexp.t)] pairs for every node,
    sorted by alias for stable output in expect tests. *)
let node_states sim =
  Sim.get_nodes sim
  |> List.map ~f:(fun n -> (Sim.N.alias_of n, Sim.N.sexp_of_role_state n))
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)

(** Walk a role_state Sexp.t and find the value of a named field.
    e.g. [find_field "proposer" sexp] returns the sexp bound to that key. *)
let find_field name (sexp : Sexp.t) =
  match sexp with
  | List fields ->
      List.find_map fields ~f:(function
        | List [Atom k; v] when String.equal k name ->
            Some v
        | _ ->
            None )
  | _ ->
      None

(** Extract the proposer sub-state sexp from a role_state sexp. *)
let extract_proposer_state role_sexp =
  match find_field "proposer" role_sexp with
  | Some s ->
      s
  | None ->
      failwith
        (Printf.sprintf "No proposer field in sexp: %s"
           (Sexp.to_string role_sexp) )

(** Extract the learner sub-state sexp from a role_state sexp. *)
let extract_learner_state role_sexp =
  match find_field "learner" role_sexp with
  | Some s ->
      s
  | None ->
      failwith
        (Printf.sprintf "No learner field in sexp: %s"
           (Sexp.to_string role_sexp) )

(** [true] if the proposer sub-state is Decided. *)
let is_decided_state role_sexp =
  match extract_proposer_state role_sexp with
  | List (Atom "Decided" :: _) ->
      true
  | _ ->
      false

(** [true] if the learner sub-state has at least one learned assertion. *)
let is_learned_state role_sexp =
  match extract_learner_state role_sexp with
  | List [Atom "Learned"; List (_ :: _)] ->
      true (* non-empty list *)
  | _ ->
      false

(** Returns only the [(alias, state_str)] pairs for nodes that have decided. *)
let decided_nodes sim =
  node_states sim |> List.filter ~f:(fun (_, s) -> is_decided_state s)

(** Returns only the [(alias, state_str)] pairs for nodes that have learned. *)
let learned_nodes sim =
  node_states sim |> List.filter ~f:(fun (_, s) -> is_learned_state s)

let print_node_states sim =
  Sim.get_nodes sim
  |> List.map ~f:(fun n ->
      let alias = Sim.N.alias_of n in
      let role_sexp = Sim.N.sexp_of_role_state n in
      let state_str = Sexp.to_string_hum role_sexp in
      let pretty_lines =
        state_str |> String.split_lines
        |> List.map ~f:(fun line -> "  " ^ String.rstrip line)
      in
      (alias, String.concat_lines pretty_lines) )
  |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.iter ~f:(fun (alias, pretty_state) ->
      Stdio.printf "Node %s:\n%s\n\n" alias pretty_state )

(** Counts how many nodes have entered the [Decided] state. *)
let count_decided sim = decided_nodes sim |> List.length

let count_learned sim = learned_nodes sim |> List.length

(** Extract the string value from a Decided sexp.
    Input shape: (Decided "some value") *)
let decided_value_of (proposer_sexp : Sexp.t) : string option =
  match proposer_sexp with List [Atom "Decided"; Atom v] -> Some v | _ -> None

(** Extract the string value from a Learned sexp.
    Input shape: (Learned (((proposal (...)) (value "some value")) ...))
    Returns the value from the first assertion in the list. *)
let learned_value_of (learner_sexp : Sexp.t) : string option =
  match learner_sexp with
  | List [Atom "Learned"; List (first_assertion :: _)] -> (
    match find_field "value" first_assertion with
    | Some (Atom v) ->
        Some v
    | _ ->
        None )
  | _ ->
      None

(** Extract the value string from the most recent (first) assertion in a
    Learned sexp.
    Input shape: (Learned (((proposal (...)) (value "v")) ...))
    The list is prepended so index 0 is always the most recent. *)
let latest_learned_value (learner_sexp : Sexp.t) : string option =
  match learner_sexp with
  | List [Atom "Learned"; List (first_assertion :: _)] -> (
    match find_field "value" first_assertion with
    | Some (Atom v) ->
        Some v
    | _ ->
        None )
  | _ ->
      None

(** Checks that every node in the cluster agrees on the same decided value.

    Returns:
    - [None]        if no node has decided yet
    - [Some (value, true)]  if all decided nodes agree
    - [Some (value, false)] if there is a disagreement (safety violation!) *)
let all_agree sim =
  match decided_nodes sim with
  | [] ->
      None
  | (_, first_role) :: rest ->
      let first_val =
        first_role |> extract_proposer_state |> decided_value_of
      in
      let all_same =
        List.for_all rest ~f:(fun (_, rs) ->
            let v = rs |> extract_proposer_state |> decided_value_of in
            Option.equal String.equal first_val v )
      in
      Some (Option.value first_val ~default:"<unknown>", all_same)

(** Checks that every node in the cluster has learned the same value.

    Returns:
    - [None]        if no node has learned yet
    - [Some (value, true)]  if all learned nodes agree
    - [Some (value, false)] if there is a disagreement (safety violation!) *)
let all_learned sim =
  match learned_nodes sim with
  | [] ->
      None
  | (_, first_role) :: rest ->
      let first_learner = extract_learner_state first_role in
      let first_val = latest_learned_value first_learner in
      let all_same =
        List.for_all rest ~f:(fun (_, rs) ->
            let v = rs |> extract_learner_state |> latest_learned_value in
            Option.equal String.equal first_val v )
      in
      Some (Option.value first_val ~default:"<unknown>", all_same)

(** Pretty-prints the decided state of every node for use in [%expect] blocks.
    Format: one line per node: "<alias>: Decided / <alias>: Pending" *)
let print_decision_summary sim =
  node_states sim
  |> List.iter ~f:(fun (alias, state) ->
      let label = if is_decided_state state then "Decided" else "Pending" in
      Stdio.printf "%s: %s\n" alias label )

(** Pretty-prints the learned state of every node for use in [%expect] blocks.
    Format: one line per node: "<alias>: Learned / <alias>: Pending" *)
let print_learned_summary sim =
  node_states sim
  |> List.iter ~f:(fun (alias, state) ->
      let label = if is_learned_state state then "Learned" else "Pending" in
      Stdio.printf "%s: %s\n" alias label )

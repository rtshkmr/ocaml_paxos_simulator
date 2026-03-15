open Base
open Log_types

module Logging_backend = struct
  type t = Stdout | Silent | Custom of (string -> unit)

  let emit ~backend s =
    match backend with
    | Stdout -> Stdio.print_endline s
    | Silent -> ()
    | Custom f -> f s
end

module Logger = struct
  type ui_type = Legacy | Gameboy

  type t = {
    mutable level : Log_level.t;
    mutable backend : Logging_backend.t;
    mutable ui : ui_type;
    module_name : string;
  }

  let create ?(level = Log_level.Info) ?(backend = Logging_backend.Stdout)
      ?(ui = Gameboy) module_name () =
    { level; backend; module_name; ui }

  (** TODO [learning, blog] Dynamic dispatch using first class modules?*)
  let resolve_ui : ui_type -> (module Ui.S) = function
    | Legacy -> (module Tui_legacy.Legacy_ui)
    | Gameboy -> (module Tui_gameboy.Gameboy_ui)

  let set_ui t ui = t.ui <- ui
  let set_level t level = t.level <- level
  let get_level t = t.level
  let set_backend t backend = t.backend <- backend

  let emit ?(node_id = None) ?(alias : string option = None)
      ?(ignore_header = false) t ~level event =
    if Log_level.should_log ~local_level:level ~global_level:t.level then
      let entry =
        Entry.make ~level ~event ?node_id ?alias ~module_name:t.module_name ()
      in
      let module UI = (val resolve_ui t.ui : Ui.S) in
      entry
      |> UI.format_entry ~ignore_header
      |> Logging_backend.emit ~backend:t.backend

  let publish_broadcast ~bus_id ?node_id ?alias t ~topic_s ~payload =
    emit ?node_id ?alias t ~level:Info
      (Log_event.Publish_broadcast { bus_id; topic_s; payload })

  let publish_unicast ~bus_id ?node_id ?alias t ~target_node ~topic_s ~payload =
    let sender_id_s = Option.map node_id ~f:Int.to_string in
    emit ~node_id ~alias t ~level:Info
      (Log_event.Publish_unicast
         {
           bus_id;
           sender_id_s;
           sender_alias = alias;
           target_node;
           topic_s;
           payload;
         })

  let subscribe ~node_id ~node_alias t ~bus_id ~topic_s ~sub_id =
    emit ~node_id:(Some node_id) ~alias:(Some node_alias) t ~level:Debug
      (Log_event.Subscribe
         { bus_id; topic_s; alias = node_alias; node_id; sub_id })

  let unsubscribe ~node_id ~alias t ~bus_id ~topic_s ~sub_id =
    emit ~node_id:(Some node_id) ~alias:(Some alias) t ~level:Debug
      (Log_event.Unsubscribe { bus_id; topic_s; alias; node_id; sub_id })

  let enqueue ~bus_id ?node_id t ~topic_s ~queue_size ~alias =
    emit ?node_id ~alias:(Some alias) t ~level:Info
      (Log_event.Enqueue { bus_id; topic_s; queue_size; alias })

  let drain_start ~bus_id ?node_id ?alias t ~batch_size =
    emit ?node_id ?alias t ~level:Debug
      (Log_event.Drain_start { bus_id; batch_size })

  let drain_end ~bus_id ~batch_size ?node_id ?alias t =
    emit ?node_id ?alias t ~level:Debug
      (Log_event.Drain_end { bus_id; batch_size })

  let tick ?node_id ?alias t ~timestamp ?(msg = "") () =
    emit ?node_id ?alias t ~level:Info
      (Log_event.Tick { tick = timestamp; msg = Some msg })

  let subroutine_flow ?node_id ?alias t ~routine ?(msg = "") () =
    emit ~node_id ~alias t ~ignore_header:true ~level:Debug
      (Log_event.Subroutine_flow { routine; msg = Some msg; node_id; alias })

  let decision ?node_id ?alias t ~msg =
    emit ~node_id ~alias t ~level:Info
      (Log_event.Decision { alias; node_id; msg })

  let reaction ?node_id ?alias t ~msg =
    emit ~node_id ~alias t ~level:Info
      (Log_event.Reaction { alias; node_id; msg })

  let node_state_change ~node_id ?alias t ~old_state ~new_state =
    emit ~node_id:(Some node_id) ~alias t ~level:Debug
      (Log_event.Node_state_change { alias; node_id; old_state; new_state })

  let display_scenario_preamble ~scenario_name ~preamble t =
    { scenario_name; preamble }
    |> Log_event.Display_scenario_preamble |> Log_event.Display
    |> emit ~ignore_header:true t ~level:Info

  let display_narration ~time ~narration t =
    { time; narration } |> Log_event.Narration |> Log_event.Display
    |> emit t ~level:Info ~ignore_header:true

  let slash_cmd_help t =
    Log_event.Help |> Log_event.Inspection |> emit t ~level:Info

  let inspect_sim_state t ~time ~partitions ~nodes =
    { time; partitions; nodes }
    |> Log_event.Sim_state |> Log_event.Sim_inspection |> Log_event.Inspection
    |> emit t ~level:Info

  let inspect_node_state t ~node_id ~alias ~dump =
    { alias; node_id; dump } |> Log_event.Node_state
    |> Log_event.Node_inspection |> Log_event.Inspection |> emit t ~level:Info

  let inspect_node_config ~node_id ~alias t ~dump =
    { alias; node_id; dump } |> Log_event.Node_config
    |> Log_event.Node_inspection |> Log_event.Inspection |> emit t ~level:Info

  let inspect_bus_stats ~bus_id
      ~(topic_stats : Log_types.Log_event.topic_stat list) t =
    { bus_id; topic_stats } |> Log_event.Bus_stats |> Log_event.Bus_inspection
    |> Log_event.Inspection |> emit t ~level:Info

  let log_proposal_action ~id ~alias ~assertion t =
    { proposer_id = id; proposer_alias = alias; assertion }
    |> Log_event.Propose |> Log_event.Paxos_action |> emit t ~level:Info

  let log_suggestion_action ~id ~alias ~assertion t =
    { proposer_id = id; proposer_alias = alias; assertion }
    |> Log_event.Suggest |> Log_event.Paxos_action |> emit t ~level:Info

  let log_announce_decided_action ~id ~alias ~assertion t =
    { proposer_id = id; proposer_alias = alias; assertion }
    |> Log_event.AnnounceDecided |> Log_event.Paxos_action |> emit t ~level:Info

  let other ?node_id ?alias t ~msg =
    emit ?node_id ?alias t ~level:Info (Log_event.Other msg)
end

(** %%%% Custom Event-Based Structured Logging System %%%%

  This module implements a **domain-specific logging system** for the Paxos
    simulator for v0 of the project.

    Instead of generic log levels (DEBUG, INFO, WARN, ERROR), we use a structured event type that captures Paxos-specific semantics (see log_types.ml).

  ## Design: Domain-Specific vs. Industry Standard

  ### What We Built Here

    Events are first-class values:
    ```ocaml

    type Log_event.t =
      | Publish_broadcast of {bus_id: int; topic_s: string; payload: string}
      | Decision of {alias: string option; node_id: int option; msg: string}
      | Node_state_change of {alias: string option; node_id: int; old: string; new_state: string}
      | ... (* ~15 variants for Paxos concepts *)

    ```

  Each event carries semantic information (node ID, alias, assertion) instead
  of a generic format string like "INFO: Alice decided X".

  ### Why Pick This Approach?

  #### Strengths

  1. **Type safety**: Log calls cannot pass invalid data. The compiler prevents typos.

  2. **Domain clarity**: "Decision" reads better than "INFO: state=decided". The event
     type explicitly documents what simulator events can happen.

  3. **Rendering flexibility**: Different backends (Gameboy TUI, Legacy TUI) format
     the same events differently. The events are independent of rendering.

  4. **Determinism in tests**: Events are data, so logging is side-effect-free from
     a *logical* perspective (the data is structured, not a string).

  #### Limitations vs. Industrial Logging

  Compared to established libraries (Core.Logger (OCaml), Serilog (.NET), Bunyan (elixir)):

  1. **No hierarchical context**: Core.Logger lets us create nested loggers:
     ```ocaml
     let proposer_log = logger |> Logger.with_scope "proposer_state" in
     ```
     We have flat context (just module_name). Nesting would require extending Logger.t.

  2. **No structured export**: Our logs are pretty-printed strings, not JSON.
     Real systems export `{"event": "decision", "node_id": 0, "assertion": "..."}`.
     This makes logs queryable: grep for all decisions by node 0.

  3. **No unification of call sites**: Log calls are scattered:
     - Logger.publish_broadcast (in Event_bus)
     - Logger.decision (in Node)
     - Logger.log_proposal_action (in Node)
     Each wraps Logger.emit differently, inconsistently.

    4. **The god object problem** (classic code smell):
      Log_event.t has ~15 variants. Each adds a new case:
     - Adding "Timeout" event requires updating Log_event, Entry, Ui.format_entry, etc.
     - Compounds over time; becomes a bottleneck.

  ## Why Not Use an Established Library

  We were aware of alternatives (Core.Logger, Serilog, structured logging patterns).
  Reasons for hand-rolling:

  1. **Time**: Learning Core.Logger's async model and context propagation would have
     taken hours we didn't have.

  2. **Domain fit**: Generic libraries treat logs as strings: "INFO: consensus reached".
     We wanted logs to be Paxos values: Decision {node_id; assertion; ...}.
     This forced us to choose: generic or domain-specific.

  3. **Pedagogical intent**: For a teaching simulation, domain-specific events are
     more valuable than production-grade aggregation infrastructure.

  The trade-off was deliberate: simplicity + clarity over reusability + scalability.

  However, spending some time looking at prior art might have given better approaches to mitigate some of the problems.

  ## Future Improvements (If Domain Grows)

  If this system needs to evolve:

  ### Stage 1: Structured Export (Easy)
  ─────────────────────────────────

  Add JSON/S-expression output:

    let event_to_json (e : Log_event.t) : Yojson.Safe.t = ...

  This enables:
    - Logging to files for post-analysis
    - Streaming to structured log tools (ELK, Datadog, etc.)
    - Filtering by field ("all events from node 0")

  ### Stage 2: Consolidate Log Calls (Medium)
  ───────────────────────────────────────────

  Currently ~20 log_* functions scattered across Node, Event_bus, Simulator.
  Each has slightly different behavior:

    ```ocaml
    Logger.publish_broadcast ~bus_id ~topic_s ~payload
    Logger.decision ~node_id ~alias ~msg
    Logger.log_reached_grant_quorum (special logic inside)
    ```

  Consolidate into a single `log` function:

    ```ocaml
    let log logger (entry : Log_entry.t) =
      Logger.emit logger ~node_id:entry.node_id ~event:entry.event
    ```

  Benefit: consistent interface, easier to extend, less duplicated code.

  ### Stage 3: Hierarchical Context (Hard)
  ────────────────────────────────────────

  Add nested loggers with inherited context (inspired by Core.Logger):

   ```ocaml
    type t = {
      ...
      mutable context: (string * string) list;
    }

    let with_context logger ~key ~data = ...
    ```
  Usage:

    ```ocaml
    let proposer_log = logger |> Log.with_context ~key:"role" ~data:"proposer" in
    let waiting_log = proposer_log |> Log.with_context ~key:"state" ~data:"waiting" in
    ```

  Benefit: automatic context propagation, less boilerplate at log sites.

  ### Stage 4: Study Prior Art (Optional Learning)
  ────────────────────────────────────────────────

  Explore how established systems solved this:

    - Core.Logger (Jane Street): Hierarchical, printf-style, implicit context
    - Serilog (.NET): Structured fields, JSON pipeline, composable
    - OpenTelemetry: Distributed tracing, span context, baggage propagation
    - ELK Stack: Machine-parseable logs, Kibana visualization

  Each represents different design choices. Our system is closest to Core.Logger
  in spirit (domain-aware) but lighter weight.

  ## The Broader Design Lesson

  The real question was never "use a library or build custom?" It was:

    **"Should logs be optimized for human reading or machine parsing?"**
    **"Generic log levels (INFO, DEBUG) or domain concepts (Decision, Suggestion)?"**

  Industry standard answers: human + generic (for broad applicability).
  Our answer: human + domain-specific (for teaching clarity).

  Both are valid; the key is **conscious choice** and **documenting the trade-off**.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

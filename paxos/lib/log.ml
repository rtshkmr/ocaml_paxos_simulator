open Base
open Log_types

module Logging_backend = struct
  type t = Stdout | Silent | Custom of (string -> unit)

  let emit ~backend s =
    match backend with
    | Stdout ->
        Stdio.print_endline s
    | Silent ->
        ()
    | Custom f ->
        f s
end

module Logger = struct
  type ui_type = Legacy | Gameboy

  type t =
    { mutable level: Log_level.t
    ; mutable backend: Logging_backend.t
    ; mutable ui: ui_type
    ; module_name: string }

  let create ?(level = Log_level.Info) ?(backend = Logging_backend.Stdout)
      ?(ui = Gameboy) module_name () =
    {level; backend; module_name; ui}

  (** TODO [learning, blog] Dynamic dispatch using first class modules?*)
  let resolve_ui : ui_type -> (module Ui.S) = function
    | Legacy ->
        (module Tui_legacy.Legacy_ui)
    | Gameboy ->
        (module Tui_gameboy.Gameboy_ui)

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
      (Log_event.Publish_broadcast {bus_id; topic_s; payload})

  let publish_unicast ~bus_id ?node_id ?alias t ~target_node ~topic_s ~payload =
    let sender_id_s = Option.map node_id ~f:Int.to_string in
    emit ~node_id ~alias t ~level:Info
      (Log_event.Publish_unicast
         { bus_id
         ; sender_id_s
         ; sender_alias= alias
         ; target_node
         ; topic_s
         ; payload } )

  let subscribe ~node_id ~node_alias t ~bus_id ~topic_s ~sub_id =
    emit ~node_id:(Some node_id) ~alias:(Some node_alias) t ~level:Debug
      (Log_event.Subscribe {bus_id; topic_s; alias= node_alias; node_id; sub_id})

  let unsubscribe ~node_id ~alias t ~bus_id ~topic_s ~sub_id =
    emit ~node_id:(Some node_id) ~alias:(Some alias) t ~level:Debug
      (Log_event.Unsubscribe {bus_id; topic_s; alias; node_id; sub_id})

  let enqueue ~bus_id ?node_id t ~topic_s ~queue_size ~alias =
    emit ?node_id ~alias:(Some alias) t ~level:Info
      (Log_event.Enqueue {bus_id; topic_s; queue_size; alias})

  let drain_start ~bus_id ?node_id ?alias t ~batch_size =
    emit ?node_id ?alias t ~level:Debug
      (Log_event.Drain_start {bus_id; batch_size})

  let drain_end ~bus_id ~batch_size ?node_id ?alias t =
    emit ?node_id ?alias t ~level:Debug
      (Log_event.Drain_end {bus_id; batch_size})

  let tick ?node_id ?alias t ~timestamp ?(msg = "") () =
    emit ?node_id ?alias t ~level:Info
      (Log_event.Tick {tick= timestamp; msg= Some msg})

  let subroutine_flow ?node_id ?alias t ~routine ?(msg = "") () =
    emit ~node_id ~alias t ~ignore_header:true ~level:Debug
      (Log_event.Subroutine_flow {routine; msg= Some msg; node_id; alias})

  let decision ?node_id ?alias t ~msg =
    emit ~node_id ~alias t ~level:Info (Log_event.Decision {alias; node_id; msg})

  let reaction ?node_id ?alias t ~msg =
    emit ~node_id ~alias t ~level:Info (Log_event.Reaction {alias; node_id; msg})

  let node_state_change ~node_id ?alias t ~old_state ~new_state =
    emit ~node_id:(Some node_id) ~alias t ~level:Debug
      (Log_event.Node_state_change {alias; node_id; old_state; new_state})

  let display_scenario_preamble ~scenario_name ~preamble t =
    {scenario_name; preamble} |> Log_event.Display_scenario_preamble
    |> Log_event.Display
    |> emit ~ignore_header:true t ~level:Info

  let display_narration ~time ~narration t =
    {time; narration} |> Log_event.Narration |> Log_event.Display
    |> emit t ~level:Info ~ignore_header:true

  let slash_cmd_help t =
    Log_event.Help |> Log_event.Inspection |> emit t ~level:Info

  let inspect_sim_state t ~time ~partitions ~nodes =
    {time; partitions; nodes} |> Log_event.Sim_state |> Log_event.Sim_inspection
    |> Log_event.Inspection |> emit t ~level:Info

  let inspect_node_state t ~node_id ~alias ~dump =
    {alias; node_id; dump} |> Log_event.Node_state |> Log_event.Node_inspection
    |> Log_event.Inspection |> emit t ~level:Info

  let inspect_node_config ~node_id ~alias t ~dump =
    {alias; node_id; dump} |> Log_event.Node_config |> Log_event.Node_inspection
    |> Log_event.Inspection |> emit t ~level:Info

  let inspect_bus_stats ~bus_id
      ~(topic_stats : Log_types.Log_event.topic_stat list) t =
    {bus_id; topic_stats} |> Log_event.Bus_stats |> Log_event.Bus_inspection
    |> Log_event.Inspection |> emit t ~level:Info

  let log_proposal_action ~id ~alias ~assertion t =
    {proposer_id= id; proposer_alias= alias; assertion}
    |> Log_event.Propose |> Log_event.Paxos_action |> emit t ~level:Info

  let log_suggestion_action ~id ~alias ~assertion t =
    {proposer_id= id; proposer_alias= alias; assertion}
    |> Log_event.Suggest |> Log_event.Paxos_action |> emit t ~level:Info

  let log_announce_decided_action ~id ~alias ~assertion t =
    {proposer_id= id; proposer_alias= alias; assertion}
    |> Log_event.AnnounceDecided |> Log_event.Paxos_action |> emit t ~level:Info

  let other ?node_id ?alias t ~msg =
    emit ?node_id ?alias t ~level:Info (Log_event.Other msg)
end

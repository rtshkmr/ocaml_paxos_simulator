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

  (* TODO: [LOG] wire up the propagation from the sim level (global prop) *)
  let create ?(level = Log_level.Debug) ?(backend = Logging_backend.Stdout)
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

  let should_log t lvl = Log_level.compare lvl t.level >= 0

  let emit ?(node_id = None) ?(alias : string option = None)
      ?(ignore_header = false) t ~level event =
    if should_log t level then
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
    emit ~node_id:(Some node_id) ~alias:(Some node_alias) t ~level:Info
      (Log_event.Subscribe {bus_id; topic_s; alias= node_alias; node_id; sub_id})

  let unsubscribe ~node_id ~alias t ~bus_id ~topic_s ~sub_id =
    emit ~node_id:(Some node_id) ~alias:(Some alias) t ~level:Info
      (Log_event.Unsubscribe {bus_id; topic_s; alias; node_id; sub_id})

  let enqueue ~bus_id ?node_id t ~topic_s ~queue_size ~alias =
    emit ?node_id ~alias:(Some alias) t ~level:Debug
      (Log_event.Enqueue {bus_id; topic_s; queue_size; alias})

  let drain_start ~bus_id ?node_id ?alias t ~batch_size =
    emit ?node_id ?alias t ~level:Info
      (Log_event.Drain_start {bus_id; batch_size})

  let drain_end ~bus_id ~batch_size ?node_id ?alias t =
    emit ?node_id ?alias t ~level:Info (Log_event.Drain_end {bus_id; batch_size})

  let tick ?node_id ?alias t ~timestamp ?(msg = "") () =
    emit ?node_id ?alias t ~level:Info
      (Log_event.Tick {tick= timestamp; msg= Some msg})

  let subroutine_flow ?node_id ?alias t ~routine ?(msg = "") () =
    emit ~node_id ~alias t ~level:Info
      (Log_event.Subroutine_flow {routine; msg= Some msg; node_id; alias})

  let decision ?node_id ?alias t ~msg =
    emit ~node_id ~alias t ~level:Info (Log_event.Decision {alias; node_id; msg})

  let reaction ?node_id ?alias t ~msg =
    emit ~node_id ~alias t ~level:Info (Log_event.Reaction {alias; node_id; msg})

  let node_state_change ~node_id ?alias t ~old_state ~new_state =
    emit ~node_id:(Some node_id) ~alias t ~level:Debug
      (Log_event.Node_state_change {alias; node_id; old_state; new_state})

  let display_scenario_preamble ~scenario_name ~preamble t =
    emit ~ignore_header:true t ~level:Info
      (Log_event.Display_scenario_preamble {scenario_name; preamble})

  let stats ~bus_id ?node_id ?alias t ~dump =
    emit ?node_id ?alias t ~level:Info (Log_event.Stats {bus_id; dump})

  let other ?node_id ?alias t ~msg =
    emit ?node_id ?alias t ~level:Info (Log_event.Other msg)
end

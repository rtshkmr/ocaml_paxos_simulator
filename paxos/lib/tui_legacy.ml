open Base
open Log_types

module Legacy_ui : Ui.S = struct
  module F = Ansi.Formatter

  let make_fenced_tag ?(bg_color = F.bg_pastel_red)
      ?(fg_color = F.fg_pastel_red) tag =
    let width = F.get_terminal_width () in
    let header_msg = tag |> bg_color |> fg_color |> F.bold in
    let fence = String.make width ' ' |> bg_color |> fg_color |> F.bold in
    (header_msg, fence)

  let format_publish_broadcast_event bus_id topic_s payload =
    let bg_color, fg_color = (F.bg_pastel_rose, F.fg_muted_plum) in
    let tag, fence =
      "{ PUBLISH_BROADCAST }" |> F.pad_string 1
      |> make_fenced_tag ~fg_color ~bg_color
    in
    let topic_str =
      topic_s |> F.pad_string 1 |> F.bold |> bg_color |> fg_color
    in
    let bus_str =
      bus_id |> Int.to_string_hum |> F.pad_string 1 |> F.bold |> bg_color
      |> fg_color
    in
    let payload_label = "\nMessage Payload:" |> F.bold |> F.underline in
    Printf.sprintf "%s\n%s via topic=%s using bus=%s:\n%s\n%s\n%s" fence tag
      topic_str bus_str payload_label payload fence

  let format_publish_unicast_event bus_id target_node sender_node sender_alias
      topic_s payload =
    let bg_color, fg_color = (F.bg_pastel_powder_blue, F.fg_muted_navy) in
    let highlight =
     fun x -> x |> F.pad_string 1 |> F.bold |> bg_color |> fg_color
    in
    let tag, fence =
      "{ PUBLISH_UNICAST }" |> F.pad_string 1
      |> make_fenced_tag ~fg_color ~bg_color
    in
    let target_str = Printf.sprintf "Node %d" target_node |> highlight in
    let sender_str =
      match (sender_node, sender_alias) with
      | None, _ | _, None ->
          ""
      | Some sender_node_s, Some sender_alias_s ->
          let sender_node_tag =
            Printf.sprintf "(Node %s)" sender_node_s |> highlight
          in
          let sender_alias_tag =
            Printf.sprintf "From %s" sender_alias_s |> highlight
          in
          sender_alias_tag ^ sender_node_tag
    in
    let topic_str = topic_s |> highlight in
    let bus_str = Int.to_string bus_id |> highlight in
    let payload_label = "\nMessage Payload:" |> F.bold |> F.underline in
    Printf.sprintf
      "%s\n%s %s to target=%s via topic=%s using bus=%s:\n%s\n%s\n%s" fence tag
      sender_str target_str topic_str bus_str payload_label payload fence

  let format_subscribe_event bus_id topic_s node_id sub_id alias =
    let bg_color, fg_color = (F.bg_pastel_mint, F.fg_deep_olive) in
    let colorise =
     fun x -> x |> F.pad_string 1 |> F.bold |> bg_color |> fg_color
    in
    let event_tag = "{ SUBSCRIBED }" |> colorise in
    let topic_tag = topic_s |> colorise in
    let bus_tag = Printf.sprintf "Bus%d" bus_id in
    let node_tag =
      Printf.sprintf "%s::(Node %s)" alias (node_id |> Int.to_string_hum)
      |> colorise
    in
    let sub_tag = sub_id |> Int.to_string_hum |> colorise in
    Printf.sprintf "%s %s to %s on %s with sub_id=%s" node_tag event_tag
      topic_tag bus_tag sub_tag

  let format_unsubscribe_event bus_id topic_s node_id sub_id alias =
    let bg_color, fg_color = (F.bg_pastel_coral, F.fg_brick_red) in
    let highlight =
     fun x -> x |> F.pad_string 1 |> F.bold |> bg_color |> fg_color
    in
    let event_tag = "{ UNSUBSCRIBED }" |> highlight in
    let topic_tag = topic_s |> highlight in
    let bus_tag = Printf.sprintf "Bus%d" bus_id in
    let node_tag =
      Printf.sprintf "%s::(Node %s)" alias (node_id |> Int.to_string_hum)
      |> highlight
    in
    let sub_tag = sub_id |> Int.to_string_hum |> highlight in
    Printf.sprintf "%s %s from %s on %s with sub_id=%s" node_tag event_tag
      topic_tag bus_tag sub_tag

  let format_enqueue_event bus_id topic_s queue_size alias =
    let highlight =
     fun x ->
      x |> F.pad_string 1 |> F.bold |> F.bg_pastel_yellow |> F.fg_pastel_yellow
    in
    let event_tag = "{ ENQUEUED }" |> highlight in
    let topic_tag = topic_s |> highlight in
    let bus_tag = Printf.sprintf "Bus%d" bus_id |> highlight in
    Printf.sprintf
      "%s message enqueued by %s on %s for topic %s with queue_size=%d"
      event_tag alias bus_tag topic_tag queue_size

  let format_drain_start bus_id batch_size =
    let bg_color, fg_color = (F.bg_pastel_blue, F.fg_pastel_blue) in
    let highlight =
     fun x -> x |> F.pad_string 1 |> F.bold |> bg_color |> fg_color
    in
    let event_tag =
      "{>>> DRAIN START >>>}"
      |> F.pad_string ~char:'>' ~do_right:false 12
      |> highlight
    in
    let bus_tag = bus_id |> Printf.sprintf "Bus%d" |> highlight in
    Printf.sprintf "%s %s buffer size=%d" event_tag bus_tag batch_size

  let format_drain_end bus_id batch_size =
    let bg_color, fg_color = (F.bg_pastel_blue, F.fg_pastel_blue) in
    let highlight =
     fun x -> x |> F.pad_string 1 |> F.bold |> bg_color |> fg_color
    in
    let size_tag = Printf.sprintf "%d messages were sent out" batch_size in
    let event_tag =
      " {<<< DRAIN END <<<} "
      |> F.pad_string ~char:'<' ~do_right:false 12
      |> highlight
    in
    let bus_tag = bus_id |> Printf.sprintf "Bus%d" |> highlight in
    Printf.sprintf "%s :: %s :: %s" event_tag bus_tag size_tag

  let format_tick tick msg =
    let bg_color, fg_color = (F.bg_pastel_green, F.fg_pastel_green) in
    let highlight =
     fun x -> x |> F.pad_string 1 |> F.bold |> bg_color |> fg_color
    in
    let m = Option.value msg ~default:"" |> F.center_text in
    let tick_content =
      Printf.sprintf "{ SIM TIME = %s }" tick |> F.pad_string 4
    in
    let tick_tag = tick_content |> highlight |> F.center_text in
    let empty =
      String.make (F.get_terminal_width ()) ' '
      |> bg_color |> fg_color |> F.bold
    in
    let fence =
      String.make (F.get_terminal_width ()) '-'
      |> bg_color |> fg_color |> F.bold
    in
    let top_buff =
      String.make (tick_content |> String.length) ' '
      |> highlight |> F.center_text
    in
    Printf.sprintf "%s%s%s\n%s\n%s\n%s\n%s" empty fence empty top_buff tick_tag
      top_buff m

  let format_subroutine_flow routine msg alias node_id =
    let highlight =
     fun x ->
      x |> F.pad_string 1 |> F.bold |> F.bg_pastel_yellow |> F.fg_pastel_yellow
    in
    let identity =
      match (alias, node_id) with
      | _, None | None, _ ->
          ""
      | Some al, Some nid ->
          Printf.sprintf "::control_flow::[%s:node %d]" al nid |> highlight
    in
    let leading_mark = "|>---" |> highlight in
    let routine_s =
      Printf.sprintf "\n\t%s[%s]" leading_mark routine |> F.bold
    in
    let msg_s = Option.value msg ~default:"" |> F.italic in
    Printf.sprintf "%s%s{%s}" identity routine_s msg_s

  let format_decision alias node_id msg =
    let highlight s =
      s |> F.pad_string 1 |> F.bold |> F.muted_sage_green_bg
      |> F.dark_olive_green_fg
    in
    let identity_s =
      match (alias, node_id) with
      | _, None | None, _ ->
          ""
      | Some alias_s, Some node_id_s ->
          Printf.sprintf "::decision::[%s:node %d]" alias_s node_id_s
          |> highlight
    in
    let msg_s = msg |> F.bright_green |> F.italic in
    Printf.sprintf "%s\n{%s}" identity_s msg_s

  let format_reason alias node_id msg =
    let highlight s =
      s |> F.pad_string 1 |> F.bold |> F.light_pastel_yellow_bg
      |> F.dark_goldenrod_brown_fg
    in
    let identity_s =
      match (alias, node_id) with
      | _, None | None, _ ->
          ""
      | Some alias_s, Some node_id_s ->
          Printf.sprintf "::reason::[%s:node %d]" alias_s node_id_s |> highlight
    in
    Printf.sprintf "%s\n{%s}" identity_s msg

  let format_state_change alias node_id old_state new_state =
    let highlight s =
      s |> F.pad_string 1 |> F.bold |> F.bg_pastel_blue |> F.fg_pastel_blue
    in
    let alias_s = Option.value alias ~default:"" in
    let identity =
      Printf.sprintf "::state_change::[%s:node %d]" alias_s node_id |> highlight
    in
    let demarc_from =
      Printf.sprintf "%s OLD STATE:" alias_s
      |> F.pad_string 1 |> F.pad_string ~char:'%' 10 |> F.fg_pastel_red
      |> F.bg_pastel_red |> F.center_text
    in
    let old_state_s = old_state |> F.red in
    let new_state_s = new_state |> F.green in
    let demarc_to =
      Printf.sprintf " %s NEW STATE: " alias_s
      |> F.pad_string ~char:'%' 10 |> F.fg_pastel_green |> F.bg_pastel_green
      |> F.center_text
    in
    let change =
      Printf.sprintf "\n\n%s\n%s\n%s\n%s" demarc_from old_state_s demarc_to
        new_state_s
    in
    Printf.sprintf "%s %s" identity change

  let format_display_scenario_preamble scenario preamble =
    let desc =
      preamble |> F.italic |> F.bright_yellow |> F.center_text_multiline
    in
    let fence =
      "\t" ^ String.make 60 '%' |> F.bright_blue |> F.bold |> F.center_text
    in
    let scenario_tag =
      Printf.sprintf "::Simulation:%s::" scenario
      |> F.pad_string 1 |> F.bold |> F.bg_pastel_yellow |> F.fg_pastel_yellow
    in
    Printf.sprintf "%s\n\n%s\n%s\n%s\n\n" scenario_tag fence desc fence

  let format_narration ~time ~narration =
    Printf.sprintf "| Narration @ time %d|\n%s" time narration

  let format_display_event = function
    | Log_event.Display_scenario_preamble {scenario_name; preamble} ->
        format_display_scenario_preamble scenario_name preamble
    | Log_event.Narration {time; narration} ->
        format_narration ~time ~narration

  let format_inspection_event _inspection =
    "TODO [low-priority] add inspection support on legacy UI"

  let format_log_event = function
    | Log_event.Publish_broadcast {bus_id; topic_s; payload} ->
        format_publish_broadcast_event bus_id topic_s payload
    | Log_event.Publish_unicast
        {bus_id; sender_id_s; sender_alias; target_node; topic_s; payload} ->
        format_publish_unicast_event bus_id target_node sender_id_s sender_alias
          topic_s payload
    | Log_event.Subscribe {bus_id; topic_s; node_id; alias; sub_id} ->
        format_subscribe_event bus_id topic_s node_id sub_id alias
    | Log_event.Unsubscribe {bus_id; topic_s; node_id; alias; sub_id} ->
        format_unsubscribe_event bus_id topic_s node_id sub_id alias
    | Log_event.Enqueue {bus_id; topic_s; queue_size; alias} ->
        format_enqueue_event bus_id topic_s queue_size alias
    | Log_event.Drain_start {bus_id; batch_size} ->
        format_drain_start bus_id batch_size
    | Log_event.Drain_end {bus_id; batch_size} ->
        format_drain_end bus_id batch_size
    | Log_event.Tick {tick; msg} ->
        format_tick tick msg
    | Log_event.Subroutine_flow {routine; msg; alias; node_id} ->
        format_subroutine_flow routine msg alias node_id
    | Log_event.Decision {alias; node_id; msg} ->
        format_decision alias node_id msg
    | Log_event.Reaction {alias; node_id; msg} ->
        format_reason alias node_id msg
    | Log_event.Node_state_change {alias; node_id; old_state; new_state} ->
        format_state_change alias node_id old_state new_state
    | Log_event.Display display ->
        format_display_event display
    | Log_event.Inspection inspection ->
        format_inspection_event inspection
    | Log_event.Other s ->
        s

  let format_entry ?(ignore_header = false) (entry : Entry.t) =
    let body = format_log_event entry.event in
    if ignore_header then body
    else
      let header =
        Printf.sprintf ">>[%s] @%s %s %s"
          (Log_level.to_string entry.level)
          (Time_float_unix.format entry.time
             ~zone:(Lazy.force Time_float_unix.Zone.local)
             "%Y-%m-%dT%H:%M:%S" )
          (Option.value entry.module_name ~default:"")
          (Option.value_map entry.node_id ~default:"" ~f:Int.to_string)
      in
      Printf.sprintf "%s\n%s\n" (header |> F.blue |> F.bold) body
end

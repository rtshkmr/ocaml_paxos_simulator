open Base
open Types

module Log_level = struct
  type t = Debug | Info | Warn | Error [@@deriving sexp_of]

  let to_int = function Debug -> 0 | Info -> 1 | Warn -> 2 | Error -> 3

  let compare a b = Int.compare (a |> to_int) (b |> to_int)

  let should_log (curr : t) (min_required : t) : bool =
    (* let open Base in *)
    let compared : int = compare curr min_required in
    compared >= 0

  let of_int = function
    | 0 ->
        Debug
    | 1 ->
        Info
    | 2 ->
        Warn
    | 3 ->
        Error
    | _ ->
        Info

  let to_string = function
    | Debug ->
        "DEBUG"
    | Info ->
        "INFO"
    | Warn ->
        "WARN"
    | Error ->
        "ERROR"

  let colorizer_of t =
    let open Ansi.Formatter in
    match t with
    | Debug ->
        magenta
    | Info ->
        cyan
    | Warn ->
        fun s -> s |> bold |> bg_pastel_rose |> fg_muted_plum
    | Error ->
        bright_red
end

(** Keep events as structured data. Call sites will pass domain-serialized
   strings (for topics, payload) to avoid logger depending on domain modules. *)
module Log_event = struct
  type t =
    | Publish_broadcast of {bus_id: int; topic_s: string; payload: string}
    | Publish_unicast of
        { bus_id: int
        ; sender_id_s: string option
        ; sender_alias: string option
        ; target_node: int
        ; topic_s: string
        ; payload: string }
    | Subscribe of
        { bus_id: int
        ; topic_s: string
        ; alias: string option
        ; node_id: int
        ; sub_id: int }
    | Unsubscribe of
        { bus_id: int
        ; topic_s: string
        ; alias: string option
        ; node_id: int
        ; sub_id: int }
    | Enqueue of {bus_id: int; topic_s: string; queue_size: int}
    | Drain_start of {bus_id: int; batch_size: int}
    | Drain_end of {bus_id: int}
    | Tick of {tick: string; msg: string option}
    | Subroutine_flow of
        { routine: string
        ; msg: string option
        ; node_id: int option
        ; alias: string option }
    | Decision of {alias: string option; node_id: int option; msg: string}
    | Reaction of {alias: string option; node_id: int option; msg: string}
    | Node_state_change of
        { alias: string option
        ; node_id: int
        ; old_state: string
        ; new_state: string }
    | Stats of {bus_id: int; dump: string}
    | Display_scenario_preamble of {scenario: string; preamble: string}
    | Other of string
  [@@deriving sexp_of]
end

module Entry = struct
  type t =
    { level: Log_level.t
    ; time: Time_float_unix.t
    ; node_id: int option
    ; alias: string option
    ; module_name: string option
    ; event: Log_event.t }
  [@@deriving sexp_of]

  let make ?node_id ?alias ?module_name ~level ~event () =
    {level; time= Time_float_unix.now (); node_id; alias; module_name; event}
end

(* ---------- Formatter ------------------------------------------------- *)
module Formatter = struct
  include Ansi.Formatter

  let level_tag lvl realtime_s =
    let level =
      Printf.sprintf ">>[%s]" (lvl |> Log_level.to_string)
      |> Log_level.colorizer_of lvl
    in
    let realtime = realtime_s |> italic in
    Printf.sprintf "%s @{%s}" level realtime

  let header_of_entry e =
    let lvl = e.Entry.level in
    let m = Option.value e.Entry.module_name ~default:"" in
    let realtime_str =
      Time_float_unix.format e.Entry.time
        ~zone:(Lazy.force Time_float_unix.Zone.local)
        "%Y-%m-%d-T%H:%M:%S.%s%Z"
    in
    let id_part =
      Option.value_map e.Entry.node_id ~default:""
        ~f:(Printf.sprintf "NodeID=%d")
    in
    let alias_part =
      Option.value_map e.Entry.alias ~default:"" ~f:(Printf.sprintf " alias=%s")
    in
    Printf.sprintf "%s %s %s%s"
      (level_tag lvl realtime_str)
      m id_part alias_part

  let make_fenced_tag ?(bg_color = bg_pastel_rose)
      ?(fg_color = default_fg_color) tag =
    let width = get_terminal_width () in
    let header_msg = tag |> bg_color |> fg_color |> bold in
    let fence = String.make width ' ' |> bg_color |> fg_color |> bold in
    (header_msg, fence)

  let format_topic color topic =
    topic |> Types.sexp_of_topic
    |> Sexp.to_string_hum ~indent:1
    |> bold |> color

  let format_publish_broadcast_event bus_id topic_s payload =
    let bg_color, fg_color = (bg_pastel_rose, fg_muted_plum) in
    let tag, fence =
      "{ PUBLISH_BROADCAST }" |> pad_string 1
      |> make_fenced_tag ~fg_color ~bg_color
    in
    let topic_str = topic_s |> pad_string 1 |> bold |> bg_color |> fg_color in
    let bus_str =
      bus_id |> Int.to_string_hum |> pad_string 1 |> bold |> bg_color
      |> fg_color
    in
    let payload_label = "\nMessage Payload:" |> bold |> underline in
    Printf.sprintf "%s\n%s via topic=%s using bus=%s:\n%s\n%s\n%s" fence tag
      topic_str bus_str payload_label payload fence

  let format_publish_unicast_event bus_id target_node sender_node sender_alias
      topic_s payload =
    (* let bg_color, fg_color = (bg_bright_blue, default_fg_color) in *)
    let bg_color, fg_color = (bg_pastel_powder_blue, fg_muted_navy) in
    let highlight =
     fun x -> x |> pad_string 1 |> bold |> bg_color |> fg_color
    in
    let tag, fence =
      "{ PUBLISH_UNICAST }" |> pad_string 1
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
    let payload_label = "\nMessage Payload:" |> bold |> underline in
    Printf.sprintf
      "%s\n%s %s to target=%s via topic=%s using bus=%s:\n%s\n%s\n%s" fence tag
      sender_str target_str topic_str bus_str payload_label payload fence

  let format_subscribe_event bus_id topic_s node_id sub_id alias_opt =
    let bg_color, fg_color = (bg_pastel_mint, fg_deep_olive) in
    let colorise = fun x -> x |> pad_string 1 |> bold |> bg_color |> fg_color in
    let event_tag = "{ SUBSCRIBED }" |> colorise in
    let topic_tag = topic_s |> colorise in
    let bus_tag = Printf.sprintf "Bus%d" bus_id in
    let alias = Option.value alias_opt ~default:"" in
    let node_tag =
      Printf.sprintf "%s::(Node %s)" alias (node_id |> Int.to_string_hum)
      |> colorise
    in
    let sub_tag = sub_id |> Int.to_string_hum |> colorise in
    Printf.sprintf "%s %s to %s on %s with sub_id=%s" node_tag event_tag
      topic_tag bus_tag sub_tag

  let format_unsubscribe_event bus_id topic_s node_id sub_id alias_opt =
    let bg_color, fg_color = (bg_pastel_coral, fg_brick_red) in
    let highlight =
     fun x -> x |> pad_string 1 |> bold |> bg_color |> fg_color
    in
    let event_tag = "{ UNSUBSCRIBED }" |> highlight in
    let topic_tag = topic_s |> highlight in
    let bus_tag = Printf.sprintf "Bus%d" bus_id in
    let alias = Option.value alias_opt ~default:"" in
    let node_tag =
      Printf.sprintf "%s::(Node %s)" alias (node_id |> Int.to_string_hum)
      |> highlight
    in
    let sub_tag = sub_id |> Int.to_string_hum |> highlight in
    Printf.sprintf "%s %s from %s on %s with sub_id=%s" node_tag event_tag
      topic_tag bus_tag sub_tag

  let format_enqueue_event bus_id topic_s queue_size =
    let highlight =
     fun x -> x |> pad_string 1 |> bold |> bg_pastel_yellow |> fg_pastel_yellow
    in
    let event_tag = "{ ENQUEUED }" |> highlight in
    let topic_tag = topic_s |> highlight in
    let bus_tag = Printf.sprintf "Bus%d" bus_id |> highlight in
    Printf.sprintf "%s message enqueued on %s for topic %s with queue_size=%d"
      event_tag bus_tag topic_tag queue_size

  let format_drain_start bus_id batch_size =
    let bg_color, fg_color = (bg_pastel_blue, fg_pastel_blue) in
    let highlight =
     fun x -> x |> pad_string 1 |> bold |> bg_color |> fg_color
    in
    let event_tag =
      "{>>> DRAIN START >>>}"
      |> pad_string ~char:'>' ~do_right:false 12
      |> highlight
    in
    let bus_tag = bus_id |> Printf.sprintf "Bus%d" |> highlight in
    Printf.sprintf "%s %s buffer size=%d" event_tag bus_tag batch_size

  let format_drain_end bus_id =
    let bg_color, fg_color = (bg_pastel_blue, fg_pastel_blue) in
    let highlight =
     fun x -> x |> pad_string 1 |> bold |> bg_color |> fg_color
    in
    let event_tag =
      " {<<< DRAIN END <<<} "
      |> pad_string ~char:'<' ~do_right:false 12
      |> highlight
    in
    let bus_tag = bus_id |> Printf.sprintf "Bus%d" |> highlight in
    Printf.sprintf "%s :: %s" event_tag bus_tag

  let format_tick tick msg =
    let bg_color, fg_color = (bg_pastel_green, fg_pastel_green) in
    let highlight =
     fun x -> x |> pad_string 1 |> bold |> bg_color |> fg_color
    in
    let m = Option.value msg ~default:"" |> center_text in
    let tick_content =
      Printf.sprintf "{ SIM TIME = %s }" tick |> pad_string 4
    in
    let tick_tag = tick_content |> highlight |> center_text in
    let empty =
      String.make (get_terminal_width ()) ' ' |> bg_color |> fg_color |> bold
    in
    let fence =
      String.make (get_terminal_width ()) '-' |> bg_color |> fg_color |> bold
    in
    let top_buff =
      String.make (tick_content |> String.length) ' '
      |> highlight |> center_text
    in
    Printf.sprintf "%s%s%s\n%s\n%s\n%s\n%s" empty fence empty top_buff tick_tag
      top_buff m

  let format_subroutine_flow routine msg alias node_id =
    let highlight =
     fun x -> x |> pad_string 1 |> bold |> bg_pastel_yellow |> fg_pastel_yellow
    in
    let identity =
      match (alias, node_id) with
      | _, None | None, _ ->
          ""
      | Some al, Some nid ->
          Printf.sprintf "::control_flow::[%s:node %d]" al nid |> highlight
    in
    let leading_mark = "|>---" |> highlight in
    let routine_s = Printf.sprintf "\n\t%s[%s]" leading_mark routine |> bold in
    let msg_s = Option.value msg ~default:"" |> italic in
    Printf.sprintf "%s%s{%s}" identity routine_s msg_s

  let format_decision alias node_id msg =
    let highlight s =
      s |> pad_string 1 |> bold |> muted_sage_green_bg |> dark_olive_green_fg
    in
    let identity_s =
      match (alias, node_id) with
      | _, None | None, _ ->
          ""
      | Some alias_s, Some node_id_s ->
          Printf.sprintf "::decision::[%s:node %d]" alias_s node_id_s
          |> highlight
    in
    let msg_s = msg |> bright_green |> italic in
    Printf.sprintf "%s\n{%s}" identity_s msg_s

  let format_reason alias node_id msg =
    let highlight s =
      s |> pad_string 1 |> bold |> light_pastel_yellow_bg
      |> dark_goldenrod_brown_fg
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
      s |> pad_string 1 |> bold |> bg_pastel_blue |> fg_pastel_blue
    in
    let alias_s = Option.value alias ~default:"" in
    let identity =
      Printf.sprintf "::state_change::[%s:node %d]" alias_s node_id |> highlight
    in
    let demarc_from =
      Printf.sprintf "%s OLD STATE:" alias_s
      |> pad_string 1 |> pad_string ~char:'%' 10 |> fg_pastel_red
      |> bg_pastel_red |> center_text
    in
    let old_state_s = old_state |> red in
    let new_state_s = new_state |> green in
    let demarc_to =
      Printf.sprintf " %s NEW STATE: " alias_s
      |> pad_string ~char:'%' 10 |> fg_pastel_green |> bg_pastel_green
      |> center_text
    in
    let change =
      Printf.sprintf "\n\n%s\n%s\n%s\n%s" demarc_from old_state_s demarc_to
        new_state_s
    in
    Printf.sprintf "%s %s" identity change

  let format_display_scenario_preamble scenario preamble =
    let desc = preamble |> italic |> bright_yellow |> center_text_multiline in
    let fence =
      "\t" ^ String.make 60 '%' |> bright_blue |> bold |> center_text
    in
    let scenario_tag =
      Printf.sprintf "::Simulation:%s::" scenario
      |> pad_string 1 |> bold |> bg_pastel_yellow |> fg_pastel_yellow
    in
    Printf.sprintf "%s\n\n%s\n%s\n%s\n\n" scenario_tag fence desc fence

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
    | Log_event.Enqueue {bus_id; topic_s; queue_size} ->
        format_enqueue_event bus_id topic_s queue_size
    | Log_event.Drain_start {bus_id; batch_size} ->
        format_drain_start bus_id batch_size
    | Log_event.Drain_end {bus_id} ->
        format_drain_end bus_id
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
    | Log_event.Display_scenario_preamble {scenario; preamble} ->
        format_display_scenario_preamble scenario preamble
    | Log_event.Stats {dump; bus_id} ->
        Printf.sprintf "[STATS for bus=(%d)] %s" bus_id dump
    | Log_event.Other s ->
        s

  let format_entry ?(ignore_header = false) entry =
    let ev = format_log_event entry.Entry.event in
    if ignore_header then ev
    else
      let header = if ignore_header then "" else header_of_entry entry in
      let header_colored = header |> blue |> bold in
      Printf.sprintf "%s\n%s\n" header_colored ev
end

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
  type t =
    { mutable level: Log_level.t
    ; mutable backend: Logging_backend.t
    ; module_name: string }

  let create ?(level = Log_level.Info) ?(backend = Logging_backend.Stdout)
      module_name () =
    {level; backend; module_name}

  let set_level t level = t.level <- level

  let get_level t = t.level

  let set_backend t backend = t.backend <- backend

  let should_log t lvl = Log_level.compare lvl t.level >= 0

  let emit ?(node_id = None) ?(alias = None) ?(ignore_header = false) t ~level
      event =
    if should_log t level then
      let entry =
        Entry.make ~level ~event ?node_id ?alias ~module_name:t.module_name ()
      in
      let s = Formatter.format_entry ~ignore_header entry in
      Logging_backend.emit ~backend:t.backend s
    else ()

  let publish_broadcast ~bus_id ?node_id ?alias t ~topic_s ~payload =
    emit ?node_id ?alias t ~level:Log_level.Info
      (Log_event.Publish_broadcast {bus_id; topic_s; payload})

  let publish_unicast ~bus_id ?node_id ?alias t ~target_node ~topic_s ~payload =
    (* Flatten nested option: int option option -> string option *)
    let sender_id_s =
      node_id |> Option.join |> Option.map ~f:Int.to_string_hum
    in
    let sender_alias = alias |> Option.join in
    emit ?node_id ?alias t ~level:Log_level.Info
      (Log_event.Publish_unicast
         {bus_id; sender_id_s; sender_alias; target_node; topic_s; payload} )

  let subscribe ~node_id ?alias t ~bus_id ~topic_s ~sub_id =
    let alias_def = Option.value alias ~default:None in
    emit ~node_id:(Some node_id) ?alias t ~level:Log_level.Info
      (Log_event.Subscribe {bus_id; topic_s; alias= alias_def; node_id; sub_id})

  let unsubscribe ~node_id ?alias t ~bus_id ~topic_s ~sub_id =
    let alias_def = Option.value alias ~default:None in
    emit ~node_id:(Some node_id) ?alias t ~level:Log_level.Info
      (Log_event.Unsubscribe {bus_id; topic_s; alias= alias_def; node_id; sub_id}
      )

  let enqueue ~bus_id ?node_id ?alias t ~topic_s ~queue_size =
    emit ?node_id ?alias t ~level:Log_level.Debug
      (Log_event.Enqueue {bus_id; topic_s; queue_size})

  let drain_start ~bus_id ?node_id ?alias t ~batch_size =
    emit ?node_id ?alias t ~level:Log_level.Info
      (Log_event.Drain_start {bus_id; batch_size})

  let drain_end ~bus_id ?node_id ?alias t =
    emit ?node_id ?alias t ~level:Log_level.Info (Log_event.Drain_end {bus_id})

  let tick ?node_id ?alias t ~timestamp ?(msg = "") () =
    emit ?node_id ?alias t ~level:Log_level.Info
      (Log_event.Tick {tick= timestamp; msg= Some msg})

  let subroutine_flow ?node_id ?alias t ~routine ?(msg = "") () =
    emit ~ignore_header:true ?node_id ?alias t ~level:Log_level.Info
      (Log_event.Subroutine_flow
         { routine
         ; node_id= Option.join node_id
         ; alias= Option.join alias
         ; msg= Some msg } )

  let decision ?node_id ?alias t ~msg =
    emit ~ignore_header:true ?node_id ?alias t ~level:Log_level.Info
      (Log_event.Decision
         {node_id= node_id |> Option.join; alias= alias |> Option.join; msg} )

  let reaction ?node_id ?alias t ~msg =
    emit ~ignore_header:true ?node_id ?alias t ~level:Log_level.Info
      (Log_event.Reaction
         {node_id= node_id |> Option.join; alias= alias |> Option.join; msg} )

  let node_state_change ~(node_id : int) ?alias t ~old_state ~new_state =
    emit ~node_id:(Some node_id) ?alias t ~level:Log_level.Info
      (Log_event.Node_state_change
         {alias= Option.join alias; node_id; old_state; new_state} )

  let display_scenario_preamble ~scenario ~preamble t =
    emit ~ignore_header:true t ~level:Log_level.Info
      (Log_event.Display_scenario_preamble {scenario; preamble})

  let stats ~bus_id ?node_id ?alias t ~dump =
    emit ?node_id ?alias t ~level:Log_level.Info (Log_event.Stats {dump; bus_id})

  let other ?node_id ?alias t ~msg =
    emit ?node_id ?alias t ~level:Log_level.Info (Log_event.Other msg)

  (* Low-level log level helpers for pipe ergonomics *)
  let debug ?node_id ?alias t event =
    emit ?node_id ?alias t ~level:Log_level.Debug event

  let info ?node_id ?alias t event =
    emit ?node_id ?alias t ~level:Log_level.Info event

  let warn ?node_id ?alias t event =
    emit ?node_id ?alias t ~level:Log_level.Warn event

  let error ?node_id ?alias t event =
    emit ?node_id ?alias t ~level:Log_level.Error event
end

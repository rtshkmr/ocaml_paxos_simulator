open Base
open Log_types
module F = Ansi.Formatter

module Gameboy_ui : Ui.S = struct
  type box_chars =
    {tl: string; tr: string; bl: string; br: string; h: string; v: string}

  let utf8_round_box = {tl= "╭"; tr= "╮"; bl= "╰"; br= "╯"; h= "─"; v= "│"}

  let _ascii_box = {tl= "+"; tr= "+"; bl= "+"; br= "+"; h= "-"; v= "|"}

  let default_box = utf8_round_box

  (* Visual motifs for GameBoy feel *)
  (* TODO: FIXME: fix the variable width char usage. for now just use single width for motifs *)
  let motif_network = "▣"

  let motif_flow = "→"

  let motif_decision = "◎"

  let motif_reaction = "@"

  let motif_state = "●"

  let motif_sim = "◆"

  (* helpers using your Ansi.Formatter utilities *)
  let visible_len s = F.get_visible_length s

  let term_width () = F.get_terminal_width ()

  let to_lines s = String.split_lines s

  (* FIXED: repeat a multi-byte string n times *)
  let repeat_string s n =
    if n <= 0 then "" else String.concat (List.init n ~f:(fun _ -> s))

  (* Palette helpers - now also used for motifs *)
  let theme_transport s =
    s |> F.bg_pastel_powder_blue |> F.fg_muted_navy |> F.bold

  let theme_flow s = s |> F.bg_pastel_yellow |> F.fg_pastel_yellow |> F.bold

  let theme_decision s =
    s |> F.muted_sage_green_bg |> F.dark_olive_green_fg |> F.bold

  let theme_reaction s =
    s |> F.light_pastel_yellow_bg |> F.dark_goldenrod_brown_fg |> F.bold

  let theme_state s = s |> F.bg_pastel_blue |> F.fg_pastel_blue |> F.bold

  let theme_sim s = s |> F.bg_pastel_yellow |> F.fg_pastel_yellow |> F.bold

  let theme_narration s =
    s |> F.bg_pastel_red |> F.fg_pastel_red |> F.bold |> F.italic

  let theme_paxos_action s =
    s |> F.bg_bright_magenta |> F.fg_muted_navy |> F.bold

  (* Color motifs to match their theme *)
  let color_motif_network s = s |> F.fg_muted_navy |> F.bold

  let color_motif_flow s = s |> F.fg_pastel_yellow |> F.bold

  let color_motif_decision s = s |> F.dark_olive_green_fg |> F.bold

  let color_motif_reaction s = s |> F.dark_goldenrod_brown_fg |> F.bold

  let color_motif_state s = s |> F.fg_pastel_blue |> F.bold

  let color_motif_sim s = s |> F.fg_pastel_yellow |> F.bold

  let color_motif_narration s = s |> F.bold_bright_red

  (* Build compact header with timestamp, level, node, module *)
  let compact_timestamp_header (e : Entry.t) =
    let lvl = Log_level.to_string e.level in
    let lvl_styled =
      match e.level with
      | Debug ->
          F.dim lvl
      | Info ->
          F.blue lvl
      | Warn ->
          F.yellow lvl
      | Error ->
          F.red lvl
    in
    let m = Option.value e.module_name ~default:"" in
    let realtime_str =
      Time_float_unix.format e.time
        ~zone:(Lazy.force Time_float_unix.Zone.local)
        "%H:%M:%S"
    in
    let time_styled = realtime_str |> F.dim in
    let id_part =
      Option.value_map e.node_id ~default:"" ~f:(fun nid ->
          let alias = Option.value e.alias ~default:(Int.to_string nid) in
          Printf.sprintf "Node %d (%s)" nid alias )
    in
    let module_part =
      if String.length m > 0 then F.dim (" [" ^ m ^ "]") else ""
    in
    String.concat ~sep:"  "
      (List.filter
         ~f:(fun s -> String.length s > 0)
         [time_styled; lvl_styled; id_part; module_part] )

  let get_max_width () =
    let min_width = 60 in
    let scale = 0.53 in
    term_width () |> Float.of_int
    |> (fun w -> w *. scale)
    |> Float.to_int |> Int.max min_width

  (** this is a rudimentary way of combining then doing a primitive version of word-wrapping.
      it is slow and it is not aesthetic enough.
   *)
  let normalize_body_lines ?max_width raw_lines =
    let max_width = Option.value_or_thunk max_width ~default:get_max_width in
    let to_lines s = String.split_lines s in
    let lines = List.concat_map raw_lines ~f:to_lines in
    (* Break a single line into wrapped lines within max_width *)
    let rec wrap_line acc line =
      if visible_len line <= max_width then List.rev (line :: acc)
      else
        let rec find_break idx last_valid =
          if idx >= String.length line then last_valid
          else
            let prefix = String.sub line ~pos:0 ~len:idx in
            if visible_len prefix <= max_width then find_break (idx + 1) idx
            else last_valid
        in
        let break_pos = find_break (max_width + 1) max_width in
        let prefix = String.sub line ~pos:0 ~len:break_pos in
        let suffix =
          String.sub line ~pos:break_pos ~len:(String.length line - break_pos)
        in
        wrap_line (prefix :: acc) suffix
    in
    let wrapped_lines = List.concat_map lines ~f:(wrap_line []) in
    let max_visible =
      List.fold wrapped_lines ~init:0 ~f:(fun acc l ->
          Int.max acc (visible_len l) )
    in
    List.map wrapped_lines ~f:(fun l ->
        let vs = visible_len l in
        if vs >= max_visible then l else l ^ String.make (max_visible - vs) ' ' )

  (* Enhanced box builder with better visual hierarchy *)
  let make_enhanced_box ~box ~motif ~event_header ~body_lines ~style_motif
      ~style_header ~style_body =
    (* Build the styled header with motif *)
    let event_header = F.pad_string 1 event_header in
    let motif_styled = style_motif motif in
    let header_styled = style_header event_header in
    let full_header = motif_styled ^ " " ^ header_styled in
    (* Compute visible lengths consistently *)
    let left_padding = " " in
    let header_text = box.h ^ full_header in
    (* Use visible_len on the header text as displayed including padding *)
    let header_text_vis = visible_len header_text in
    let body_max_vis =
      match body_lines with
      | [] ->
          0
      | bs ->
          List.fold bs ~init:0 ~f:(fun acc l -> Int.max acc (visible_len l))
    in
    (* content width is max of header visible length and body visible length *)
    let content_w = Int.max header_text_vis body_max_vis in
    (* inner width accounts for content + right padding after header/body (match left padding) *)
    let inner_w = content_w + visible_len left_padding in
    (* rest length is fill to make top line full width *)
    let rest_len = inner_w - header_text_vis in
    let rest_len = if rest_len < 0 then 0 else rest_len in
    (* fill with repeated h char *)
    let hfill = repeat_string box.h rest_len in
    (* construct top line with borders and aligned padding *)
    let top = box.tl ^ header_text ^ hfill ^ box.tr in
    (* separator line *)
    let sep = box.v ^ repeat_string box.h inner_w ^ box.v in
    (* body lines with same left padding and calculated right padding *)
    let mk_body_line raw =
      let styled = style_body raw in
      let vs = visible_len raw in
      let pad_len = inner_w - visible_len left_padding - vs in
      let pad_len = if pad_len < 0 then 0 else pad_len in
      let padded = left_padding ^ styled ^ String.make pad_len ' ' in
      box.v ^ padded ^ box.v
    in
    let body_rows =
      match body_lines with
      | [] ->
          [mk_body_line " "]
      | bs ->
          List.map bs ~f:mk_body_line
    in
    (* bottom line *)
    let bottom = box.bl ^ repeat_string box.h inner_w ^ box.br in
    String.concat ~sep:"\n" ((top :: sep :: body_rows) @ [bottom])

  (* Format helpers for different event types *)
  let format_publish_broadcast_event ~bus_id ~topic_s ~payload =
    let event_header = Printf.sprintf "NETWORK: PUBLISH_BROADCAST" in
    let body_lines =
      [ Printf.sprintf "topic: %s | bus: %d" topic_s bus_id
      ; ""
      ; "Message Payload:" |> F.underline ]
      @ to_lines payload
    in
    make_enhanced_box ~box:default_box ~motif:motif_network ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_network ~style_header:theme_transport
      ~style_body:Fn.id

  let format_publish_unicast_event ~bus_id ~target_node ~sender_id_s
      ~sender_alias ~topic_s ~payload =
    let sender =
      match (sender_id_s, sender_alias) with
      | None, _ | _, None ->
          "anonymous"
      | Some id_s, Some alias_s ->
          Printf.sprintf "%s (Node %s)" alias_s id_s
    in
    let event_header =
      Printf.sprintf "NETWORK: PUBLISH_UNICAST → Node %d" target_node
    in
    let body_lines =
      [ Printf.sprintf "topic: %s  │  bus: %d  │  from: %s" topic_s bus_id sender
      ; ""
      ; "Payload:" ]
      @ to_lines payload
    in
    make_enhanced_box ~box:default_box ~motif:motif_network ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_network ~style_header:theme_transport
      ~style_body:Fn.id

  let format_subscribe_event ~bus_id ~topic_s ~node_id ~sub_id ~alias =
    let event_header =
      Printf.sprintf "NETWORK: SUBSCRIBED <%s(%02d) @ %s> " alias node_id
        topic_s
    in
    let alias_fmt = alias |> F.bold |> F.italic in
    let body_lines =
      [ Printf.sprintf
          "%s will get messages about the topic:%s (bus:%d, sub:%d)" alias_fmt
          topic_s bus_id sub_id ]
    in
    make_enhanced_box ~box:default_box ~motif:motif_network ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_network ~style_header:theme_transport
      ~style_body:Fn.id

  let format_unsubscribe_event ~bus_id ~topic_s ~node_id ~sub_id ~alias =
    let alias_fmt = alias |> F.bold |> F.italic in
    let event_header =
      Printf.sprintf "NETWORK: UNSUBSCRIBED <%s(%02d) @ %s>" alias node_id
        topic_s
    in
    let body_lines =
      [ Printf.sprintf
          "%s will no longer get messages about the topic:%s (bus:%d, sub:%d)"
          alias_fmt topic_s bus_id sub_id ]
    in
    make_enhanced_box ~box:default_box ~motif:motif_network ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_network ~style_header:theme_transport
      ~style_body:F.bold

  let format_enqueue_event ~bus_id ~topic_s ~queue_size ~alias =
    let event_header = Printf.sprintf "NETWORK: ENQUEUE" in
    let whom = alias |> F.bold in
    let body_lines =
      [ Printf.sprintf "%s submitted a message which will be sent soon." whom
      ; Printf.sprintf "by:%s | topic:%s  │  bus:%d  │  queue_size:%d" alias
          topic_s bus_id queue_size ]
    in
    make_enhanced_box ~box:default_box ~motif:motif_network ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_network ~style_header:theme_transport
      ~style_body:Fn.id

  let format_drain_start ~bus_id ~batch_size =
    let event_header = Printf.sprintf "NETWORK: DRAIN_START" in
    let body_lines =
      [Printf.sprintf "[ %03d ] messages to send for bus:%d  " batch_size bus_id]
    in
    make_enhanced_box ~box:default_box ~motif:motif_network ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_network ~style_header:theme_transport
      ~style_body:F.bold

  let format_drain_end ~bus_id ~batch_size =
    let event_header = Printf.sprintf "NETWORK: DRAIN_END" in
    let body_lines =
      [ Printf.sprintf "[ %03d ] messages were sent out on bus:%d" batch_size
          bus_id ]
    in
    make_enhanced_box ~box:default_box ~motif:motif_network ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_network ~style_header:theme_transport
      ~style_body:F.bold

  (* ENHANCED: Tick events with visual separation *)
  let format_tick ~tick ~msg =
    let bg_color, fg_color = (F.bg_pastel_green, F.fg_pastel_green) in
    let term_w = F.get_terminal_width () in
    (* Single-width UTF8 character motif *)
    let burst_char = "═" in
    let star = "✦" in
    (* safe 1-column UTF8 star *)
    (* Produce a full-width "energy line" *)
    let line_burst () =
      String.init term_w ~f:(fun _ -> String.get burst_char 0)
      |> bg_color |> fg_color |> F.bold
    in
    (* Center a piece of colored text across the terminal width *)
    let center_colored s =
      let visible = F.get_visible_length s in
      if visible >= term_w then s
      else
        let pad = (term_w - visible) / 2 in
        String.make pad ' ' ^ s
    in
    (* Highlighted SIM TIME banner *)
    let sim_label =
      Printf.sprintf "%s  SIM TIME → %s  %s" star tick star
      |> F.pad_string 10 |> F.bold |> bg_color |> fg_color |> center_colored
    in
    (* Optional centered message *)
    let msg_lines =
      match msg with
      | None | Some "" ->
          []
      | Some m ->
          let centered = F.center_text m in
          String.split_lines centered
          |> List.map ~f:(fun line ->
                 line |> bg_color |> fg_color |> F.bold |> center_colored )
    in
    String.concat ~sep:"\n"
      ([line_burst (); sim_label; line_burst ()] @ msg_lines @ [line_burst ()])

  let format_subroutine_flow ~routine ~msg ~alias ~node_id =
    let whom =
      match (alias, node_id) with
      | Some al, Some nid ->
          Printf.sprintf "%s (%02d)" al nid
      | Some al, None ->
          al
      | None, Some nid ->
          Printf.sprintf "node %02d" nid
      | None, None ->
          "local"
    in
    let event_header = Printf.sprintf "CONTROL FLOW: [@%s] %s" whom routine in
    let body_lines = match msg with None -> [] | Some m -> to_lines m in
    make_enhanced_box ~box:default_box ~motif:motif_flow ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_flow ~style_header:theme_flow
      ~style_body:F.italic

  let format_decision ~alias ~node_id ~msg =
    let who =
      Option.value alias
        ~default:(Option.value_map node_id ~default:"anon" ~f:Int.to_string)
    in
    let event_header = Printf.sprintf "DECISION: by %s" who in
    let body_lines = to_lines msg in
    make_enhanced_box ~box:default_box ~motif:motif_decision ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_decision ~style_header:theme_decision
      ~style_body:F.bright_green

  let format_reaction ~alias ~node_id ~msg =
    let who =
      Option.value alias
        ~default:(Option.value_map node_id ~default:"anon" ~f:Int.to_string)
    in
    let event_header = Printf.sprintf "REACTION: %s" who in
    let body_lines = to_lines msg in
    make_enhanced_box ~box:default_box ~motif:motif_reaction ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_reaction ~style_header:theme_reaction
      ~style_body:F.italic

  let format_state_change ~alias ~node_id ~old_state ~new_state =
    let alias_s =
      Option.value alias ~default:(Printf.sprintf "Node %d" node_id)
    in
    let event_header = Printf.sprintf "STATE: %s changed" alias_s in
    let arrow = " → " |> F.bold |> F.bright_yellow in
    let old_lines = old_state |> to_lines |> List.map ~f:F.red in
    let new_lines = new_state |> to_lines |> List.map ~f:F.green in
    let body_lines =
      match (old_lines, new_lines) with
      | [old_single], [new_single] ->
          (* Compact single-line transition *)
          [old_single ^ arrow ^ new_single]
      | _ ->
          (* Multi-line state dump with separator *)
          ["[OLD STATE:]"] @ old_lines @ ["[NEW STATE:]"] @ new_lines
    in
    make_enhanced_box ~box:default_box ~motif:motif_state ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_state ~style_header:theme_state ~style_body:Fn.id

  let format_display_scenario_preamble ~scenario_name ~preamble =
    let event_header = Printf.sprintf "SCENARIO: %s" scenario_name in
    let body_lines = to_lines preamble in
    make_enhanced_box ~box:default_box ~motif:motif_sim ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_sim ~style_header:theme_sim
      ~style_body:F.bright_yellow

  let format_narration ~time ~narration =
    let event_header = Printf.sprintf "{~NARRATION~} @ time = %d" time in
    let narration_lines = to_lines narration in
    make_enhanced_box ~box:default_box ~motif:motif_sim ~event_header
      ~body_lines:(normalize_body_lines narration_lines)
      ~style_motif:color_motif_narration ~style_header:theme_narration
      ~style_body:Fn.id

  let format_topic_stats stats =
    let header =
      "Topic Statistics Overview" |> F.bold |> F.fg_pastel_blue
      |> F.bg_pastel_powder_blue |> F.underline
    in
    let format_stat_card
        ({topic; subscribers; published; delivered; queued} :
          Log_types.Log_event.topic_stat ) =
      let topic_str = Sexp.to_string (Types.Types.sexp_of_topic topic) in
      let card_header =
        Printf.sprintf "Topic: %s" topic_str
        |> F.bg_pastel_mint |> F.black |> F.bold |> F.pad_string 1
      in
      let stats_str =
        Printf.sprintf
          "-Subscribers: %03d\n\
           -Published: %03d\n\
           -Delivered: %03d\n\
           -Queued: %03d"
          subscribers published delivered queued
      in
      let card_body = stats_str |> F.bold |> F.pad_string 1 in
      let card = Printf.sprintf "%s\n%s" card_header card_body in
      card
    in
    let topic_cards = List.map stats ~f:format_stat_card in
    [header] @ topic_cards

  let format_bus_stats ~bus_id
      ~(topic_stats : Log_types.Log_event.topic_stat list) =
    let event_header = Printf.sprintf "STATS: bus=%d" bus_id in
    let body_lines = format_topic_stats topic_stats in
    make_enhanced_box ~box:default_box ~motif:motif_network ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_network ~style_header:theme_transport
      ~style_body:Fn.id

  let format_node_inspection_event ev =
    match ev with
    | Log_event.Node_state {alias; node_id; dump} ->
        let who = Printf.sprintf "%s (node %d)" alias node_id in
        let event_header = Printf.sprintf "INSPECT: NODE STATE %s" who in
        let body_lines = to_lines dump in
        make_enhanced_box ~box:default_box ~motif:motif_state ~event_header
          ~body_lines:(normalize_body_lines body_lines)
          ~style_motif:color_motif_state ~style_header:theme_state
          ~style_body:Fn.id
    | Log_event.Node_config {alias; node_id; dump} ->
        let who = Printf.sprintf "%s (node %d)" alias node_id in
        let event_header = Printf.sprintf "INSPECT: NODE CONFIG %s" who in
        let body_lines = to_lines dump in
        make_enhanced_box ~box:default_box ~motif:motif_state ~event_header
          ~body_lines:(normalize_body_lines body_lines)
          ~style_motif:color_motif_state ~style_header:theme_state
          ~style_body:Fn.id

  let format_sim_inspection_event = function
    | Log_event.Sim_state {time; partitions; nodes} ->
        let event_header =
          Printf.sprintf "INSPECT: SIM STATE @ Time=%03d" time
        in
        let mk_header s = s |> F.underline |> F.bold in
        let partition_tag, nodes_tag =
          (mk_header "[PARTITIONS]", mk_header "[NODES]")
        in
        let body_lines =
          [partition_tag; partitions; "\n"; nodes_tag; nodes]
          |> List.concat_map ~f:to_lines
        in
        make_enhanced_box ~box:default_box ~motif:motif_sim ~event_header
          ~body_lines:(normalize_body_lines body_lines)
          ~style_motif:color_motif_sim ~style_header:theme_sim ~style_body:Fn.id

  let format_bus_inspection_event = function
    | Log_event.Bus_stats {bus_id; topic_stats} ->
        format_bus_stats ~bus_id ~topic_stats

  let format_inspection_help_banner () =
    let event_header = "INSPECT: HELP" in
    let body_lines =
      [ "Available inspection commands:" |> F.underline
      ; motif_decision
        ^ " /inspect/sim/state:\n  shows you the state of the simulation"
      ; motif_decision
        ^ " /inspect/node/state/<alias> :\n\
          \  shows you the state of a particular node"
      ; motif_decision
        ^ " /inspect/node/config/<alias>:\n\
          \  shows you the config of a particular node"
      ; motif_decision
        ^ " /inspect/bus/stats/<bus_id> :\n\
          \  shows you the current stats on the message bus" ]
    in
    make_enhanced_box ~box:default_box ~motif:motif_sim ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_sim ~style_header:theme_sim ~style_body:Fn.id

  let format_inspection_event = function
    | Log_event.Node_inspection node_ev ->
        format_node_inspection_event node_ev
    | Log_event.Sim_inspection sim_ev ->
        format_sim_inspection_event sim_ev
    | Log_event.Bus_inspection ev ->
        format_bus_inspection_event ev
    | Log_event.Help ->
        format_inspection_help_banner ()

  let format_display_event = function
    | Log_event.Display_scenario_preamble {scenario_name; preamble} ->
        format_display_scenario_preamble ~scenario_name ~preamble
    | Log_event.Narration {time; narration} ->
        format_narration ~time ~narration

  let format_paxos_proposal ~id ~alias ~assertion =
    let event_header = Printf.sprintf "PROPOSAL by %s(%02d)" alias id in
    let body_lines =
      [Printf.sprintf "%s (%02d) initiates a proposal" alias id; assertion]
    in
    make_enhanced_box ~box:default_box ~motif:motif_reaction ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_reaction ~style_header:theme_paxos_action
      ~style_body:F.italic

  let format_paxos_suggestion ~id ~alias ~assertion =
    let event_header = Printf.sprintf "SUGGESTION by %s(%02d)" alias id in
    let body_lines =
      [ Printf.sprintf
          "%s (%02d) suggested after getting permission from majority" alias id
      ; assertion ]
    in
    make_enhanced_box ~box:default_box ~motif:motif_reaction ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_reaction ~style_header:theme_paxos_action
      ~style_body:F.italic

  let format_paxos_announce_decided ~id ~alias ~assertion =
    let event_header =
      Printf.sprintf "ANNOUNCING DECIDED by %s(%02d)" alias id
    in
    let body_lines =
      [ "Success!"
      ; Printf.sprintf
          "%s (%02d) managed to achieve consensus for the following state:"
          alias id
      ; assertion ]
    in
    make_enhanced_box ~box:default_box ~motif:motif_reaction ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_reaction ~style_header:theme_paxos_action
      ~style_body:F.italic

  let format_paxos_action_event (pa : Log_event.paxos_actions) =
    match pa with
    | Propose {proposer_id: int; proposer_alias: string; assertion: string} ->
        format_paxos_proposal ~id:proposer_id ~alias:proposer_alias ~assertion
    | Suggest {proposer_id: int; proposer_alias: string; assertion: string} ->
        format_paxos_suggestion ~id:proposer_id ~alias:proposer_alias ~assertion
    | AnnounceDecided
        {proposer_id: int; proposer_alias: string; assertion: string} ->
        format_paxos_announce_decided ~id:proposer_id ~alias:proposer_alias
          ~assertion

  let format_other s =
    let event_header = "LOG" in
    let body_lines = to_lines s in
    make_enhanced_box ~box:default_box ~motif:"●" ~event_header
      ~body_lines:(normalize_body_lines body_lines)
      ~style_motif:color_motif_flow ~style_header:theme_flow ~style_body:F.bold

  let format_log_event ev =
    match ev with
    | Log_event.Publish_broadcast {bus_id; topic_s; payload} ->
        format_publish_broadcast_event ~bus_id ~topic_s ~payload
    | Log_event.Publish_unicast
        {bus_id; sender_id_s; sender_alias; target_node; topic_s; payload} ->
        format_publish_unicast_event ~bus_id ~target_node ~sender_id_s
          ~sender_alias ~topic_s ~payload
    | Log_event.Subscribe {bus_id; topic_s; alias; node_id; sub_id} ->
        format_subscribe_event ~bus_id ~topic_s ~node_id ~sub_id ~alias
    | Log_event.Unsubscribe {bus_id; topic_s; alias; node_id; sub_id} ->
        format_unsubscribe_event ~bus_id ~topic_s ~node_id ~sub_id ~alias
    | Log_event.Enqueue {bus_id; topic_s; queue_size; alias} ->
        format_enqueue_event ~bus_id ~topic_s ~queue_size ~alias
    | Log_event.Drain_start {bus_id; batch_size} ->
        format_drain_start ~bus_id ~batch_size
    | Log_event.Drain_end {bus_id; batch_size} ->
        format_drain_end ~bus_id ~batch_size
    | Log_event.Tick {tick; msg} ->
        format_tick ~tick ~msg
    | Log_event.Subroutine_flow {routine; msg; node_id; alias} ->
        format_subroutine_flow ~routine ~msg ~alias ~node_id
    | Log_event.Decision {alias; node_id; msg} ->
        format_decision ~alias ~node_id ~msg
    | Log_event.Reaction {alias; node_id; msg} ->
        format_reaction ~alias ~node_id ~msg
    | Log_event.Node_state_change {alias; node_id; old_state; new_state} ->
        format_state_change ~alias ~node_id ~old_state ~new_state
    | Log_event.Display display ->
        format_display_event display
    | Log_event.Inspection inspection ->
        format_inspection_event inspection
    | Log_event.Paxos_action pa ->
        format_paxos_action_event pa
    | Log_event.Other s ->
        format_other s

  (* Exposed API per Ui.S *)
  let format_entry ?(ignore_header = false) (entry : Entry.t) =
    let body_box = format_log_event entry.event in
    if ignore_header then body_box
    else
      let hdr = compact_timestamp_header entry in
      String.concat ~sep:"\n" [hdr; body_box; ""]
end

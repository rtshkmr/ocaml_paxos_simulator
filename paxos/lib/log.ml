module LogFormatter = struct
  open Ansi.Formatter
  open Types
  open Base
  open Core.Time_float

  let get_terminal_width () =
    let open Stdio in
    let ic = Unix.open_process_in "tput cols" in
    try
      let line = Option.value (In_channel.input_line ic) ~default:"80" in
      ignore (Unix.close_process_in ic) ;
      Int.of_string line
    with _ -> 80

  let center_string_in_terminal s =
    let width = get_terminal_width () in
    let len = String.length s in
    let pad = Int.max 0 ((width - len) / 2) in
    String.make pad ' ' ^ s

  let format_subroutine_flow routine_name msg =
    let open Printf in
    let routine_tag = sprintf "(%s)" routine_name |> yellow |> italic in
    sprintf "|>---[%s] {##%s##}" routine_tag msg

  let format_tick_msg timestamp ?(msg = "") () =
    (* Format local current time as ISO8601 *)
    let msg_str = msg |> italic in
    let iso_time_str =
      to_string_abs ~zone:(Zone.of_utc_offset ~hours:8) (now ())
      |> String.strip |> italic
    in
    timestamp
    |> fun curr_tick_str ->
    Printf.sprintf "\n\n\t\t\t\t[Clock:Tick %s] --- realtime = %s" curr_tick_str
      iso_time_str
    |> bright_green |> bold |> underline |> center_string_in_terminal
    |> fun tick_tag -> tick_tag ^ "\n\t\t\t\t" ^ msg_str

  (* Role tag formatters *)

  let role_tag node_id role_str =
    Printf.sprintf "[Node%3d::Role::%s]" node_id role_str |> blue |> bold

  let acceptor_tag node_id = "Acceptor" |> role_tag node_id

  let proposer_tag node_id = "Proposer" |> role_tag node_id

  let learner_tag node_id = "Learner" |> role_tag node_id

  (* Reaction style: italic *)
  let reaction msg = msg |> italic

  (* Decision style: dim blue italic *)
  let decision msg = msg |> blue |> dim |> italic

  (* Reusable formatter building functions: *)
  let header_tag tag header_color =
    let width = get_terminal_width () in
    let header_msg = tag |> bg_white |> header_color |> bold in
    let header = String.make width '-' |> header_color |> bold in
    (header_msg, header)

  let format_topic color topic =
    topic |> Types.sexp_of_topic
    |> Sexp.to_string_hum ~indent:1
    |> bold |> color

  let publish_broadcast topic payload =
    let header_msg, header = header_tag "[PUBLISH_BROADCAST]" red in
    let topic_str = format_topic red topic in
    Printf.sprintf "%s\n%s via topic %s:\n%s\n%s" header header_msg topic_str
      payload header

  let publish_unicast node_id topic payload =
    let label =
      Printf.sprintf "[PUBLISH_UNICAST] Target Node ID = %d" node_id
    in
    let header_msg, header = header_tag label magenta in
    let topic_str = format_topic magenta topic in
    Printf.sprintf "%s\n%s via topic %s:\n%s\n%s" header header_msg topic_str
      payload header

  let subscribe topic node_id subscription_id =
    let topic_str = Types.sexp_of_topic topic |> Sexp.to_string_hum ~indent:1 in
    let tag =
      Printf.sprintf "<node[%d]::Subscribed @ %s>" node_id topic_str
      |> green |> bold
    in
    Printf.sprintf "%s\n\ttopic=%s, node_id=%d, subscription_id=%d" tag
      topic_str node_id subscription_id

  let unsubscribe topic node_id subscription_id =
    let topic_str = Types.sexp_of_topic topic |> Sexp.to_string_hum ~indent:1 in
    Printf.sprintf
      "< --- Unsubscribed --- >:\n  topic=%s, node_id=%d, subscription_id=%d"
      topic_str node_id subscription_id
    |> red |> bold

  let enqueue topic queue_size =
    let topic_str =
      topic |> Types.sexp_of_topic
      |> Sexp.to_string_hum ~indent:1
      |> cyan |> bold
    in
    let header = "[event_bus::ENQUEUE]" |> yellow |> underline |> bold in
    Printf.sprintf "%s Enqueued message, queue size now %s for topic %s" header
      (Int.to_string queue_size |> magenta)
      topic_str

  let drain_start batch_size =
    let header = "[event_bus::DRAIN_START]" |> bright_blue |> bold in
    Printf.sprintf "%s Starting to drain a snapshotted queue of %s messages..."
      header
      (Int.to_string batch_size |> magenta |> bold)
    |> underline

  let drain_end () =
    "[event_bus::DRAIN_END] Finished draining queue." |> bright_blue |> bold
    |> underline

  let print_stats () =
    "[event_bus::STATS] Printing statistics..." |> blue |> bold

  let format_node_state_change node_id old_state_str new_state_str =
    Printf.sprintf "Node %d changed state from \n%s\n\t\t-----to-------\n%s\n%!"
      node_id old_state_str new_state_str

  (* Unified record type for composability *)
  type 'a formatters =
    { publish_broadcast: Types.topic -> string -> string
    ; publish_unicast: Types.node_id -> Types.topic -> string -> string
    ; subscribe: Types.topic -> int -> int -> string
    ; unsubscribe: Types.topic -> int -> int -> string
    ; enqueue: Types.topic -> int -> string
    ; drain_start: int -> string
    ; drain_end: unit -> string
    ; format_tick_msg: string -> ?msg:string -> unit -> string
    ; format_subroutine_flow: string -> string -> string
    ; decision: string -> string
    ; reaction: string -> string
    ; format_node_state_change: int -> string -> string -> string
    ; print_stats: unit -> string }

  let make () : 'a formatters =
    { publish_broadcast
    ; publish_unicast
    ; subscribe
    ; unsubscribe
    ; enqueue
    ; drain_start
    ; drain_end
    ; format_tick_msg
    ; format_subroutine_flow
    ; decision
    ; reaction
    ; format_node_state_change
    ; print_stats }
end

module Logger = struct
  open Stdio

  type 'a t = {formatters: 'a LogFormatter.formatters}

  let create () = {formatters= LogFormatter.make ()}

  (* Logging actions: always active *)
  let log_publish_broadcast t topic payload =
    print_endline (t.formatters.publish_broadcast topic payload)

  let log_publish_unicast t node_id topic payload =
    print_endline (t.formatters.publish_unicast node_id topic payload)

  let log_subscribe t topic node_id subscription_id =
    print_endline (t.formatters.subscribe topic node_id subscription_id)

  let log_unsubscribe t topic node_id subscription_id =
    print_endline (t.formatters.unsubscribe topic node_id subscription_id)

  let log_enqueue t topic queue_size =
    print_endline (t.formatters.enqueue topic queue_size)

  let log_drain_start t batch_size =
    print_endline (t.formatters.drain_start batch_size)

  let log_drain_end t = print_endline (t.formatters.drain_end ())

  let log_stats_header t = print_endline (t.formatters.print_stats ())

  let log_event_bus_stats t stats_dump =
    log_stats_header t ; print_endline stats_dump

  let log_tick t ?(msg = "") timestamp () =
    print_endline (t.formatters.format_tick_msg timestamp ~msg ())

  let log_subroutine_flow t subroutine ?(msg = "") () =
    print_endline (t.formatters.format_subroutine_flow subroutine msg)

  let log_decision t msg = print_endline (t.formatters.decision msg)

  let log_reaction t msg = print_endline (t.formatters.reaction msg)

  let log_node_state_change t node_id old_state_str new_state_str =
    t.formatters.format_node_state_change node_id old_state_str new_state_str
    |> print_endline
end

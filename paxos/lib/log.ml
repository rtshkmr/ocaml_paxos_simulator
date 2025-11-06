module LogFormatter = struct
  open Color.Color
  open Types
  open Base

  (* Role tag formatters *)
  let acceptor_tag = "[Acceptor]" |> blue |> bold

  let proposer_tag = "[Proposer]" |> blue |> bold

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
    Printf.sprintf
      "< +++ Subscribed +++ >:\n  topic=%s, node_id=%d, subscription_id=%d"
      topic_str node_id subscription_id
    |> green |> bold

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
    let header = "[ENQUEUE]" |> yellow |> underline |> bold in
    Printf.sprintf "%s Enqueued message, queue size now %s for topic %s" header
      (Int.to_string queue_size |> magenta)
      topic_str

  let drain_start batch_size =
    let header = "[DRAIN_START]" |> bright_blue |> bold in
    Printf.sprintf "%s Starting to drain queue of %s messages..." header
      (Int.to_string batch_size |> magenta |> bold)
    |> underline

  let drain_end () =
    "[DRAIN_END] Finished draining queue." |> bright_blue |> bold |> underline

  let print_stats () = "[STATS] Printing statistics..." |> blue |> bold

  (* Unified record type for composability *)
  type 'a formatters =
    { publish_broadcast: Types.topic -> string -> string
    ; publish_unicast: Types.node_id -> Types.topic -> string -> string
    ; subscribe: Types.topic -> int -> int -> string
    ; unsubscribe: Types.topic -> int -> int -> string
    ; enqueue: Types.topic -> int -> string
    ; drain_start: int -> string
    ; drain_end: unit -> string
    ; print_stats: unit -> string }

  let make () : 'a formatters =
    { publish_broadcast
    ; publish_unicast
    ; subscribe
    ; unsubscribe
    ; enqueue
    ; drain_start
    ; drain_end
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
end

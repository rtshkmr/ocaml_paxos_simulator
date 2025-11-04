[@@@ocaml.warning "-69"] (** TODO: remove unused variable warnings*)
open Base
open Types
open Color

(** Module type for a polymorphic type with a higher-kinded type parameter ['a t]
   ['a] is a higher-kinded type here. It needs to be fully applied for it to be used by a functor.
*)
module type S = sig
  type 'a t

  type sub_handle [@@deriving sexp, compare, equal, hash]

  val create : ?logger:(Types.topic -> 'a -> string) -> unit -> 'a t

  val subscribe :
       'a t
    -> topic:Types.topic
    -> node_id:Types.node_id
    -> ('a -> unit)
    -> sub_handle

  val unsubscribe : 'a t -> sub_handle -> unit

  val publish_broadcast : 'a t -> topic:Types.topic -> 'a -> unit

  val publish_unicast : 'a t -> topic:Types.topic -> node_id:int -> 'a -> unit


  type 'a enqueuable_thunk = ((Types.topic * Types.node_id option) * 'a)

  val enqueue : 'a t -> 'a enqueuable_thunk -> unit

  val drain : 'a t -> unit

  val stats : 'a t -> (Types.topic * (int * int * int * int)) list

  val print_stats : 'a t -> unit
end

module Event_bus : S = struct
  (** sub_handle is the type for what a subscription handle looks like.
      -  [id] here refers to a subscription id (arbitrary for now)
  *)
  type sub_handle = {topic: Types.topic; id: int; node_id: Types.node_id}
  [@@deriving sexp, compare, equal, hash]

  type 'a callback = 'a -> unit

  type 'a subscription_info =
    {node_id: Types.node_id; sub_handle: sub_handle; callback: 'a callback}

  type 'a topic_state =
    { subs: (sub_handle, 'a subscription_info) Hashtbl.t
    ; mutable published: int
    ; mutable delivered: int
    ; mutable queued: int }

  (** A thunk that can be queued such that it will either be published as a broadcast or as a publish to
      a single node *)
  type 'a enqueuable_thunk = ((Types.topic * Types.node_id option) * 'a)

  type 'a base_formatter = (Types.topic -> 'a -> string)

  type 'a t =
    { mutable next_id: int
    ; topics: (Types.topic, 'a topic_state) Hashtbl.Poly.t
    ; queue: 'a enqueuable_thunk Queue.t
    ; logger: 'a base_formatter option; }

  type 'a formatters = {
    publish_broadcast : 'a base_formatter -> Types.topic -> 'a -> string;
    publish_unicast : Types.node_id -> 'a base_formatter -> Types.topic -> 'a -> string;
    subscribe : sub_handle -> string;
    unsubscribe : sub_handle -> string;
    enqueue : Types.topic -> int -> string; (* topic and queue size *)
    (* FIXME: do something similar to print_stats withh the [LOG_TYPE] header *)
    drain_start : int -> string;
    drain_end : unit -> string;
    print_stats : unit -> string;
  }

  let formatters = {
    publish_broadcast = (fun logger topic payload ->
        let width = Color.get_terminal_width () in
        let header = String.make width '-' in
        let header_msg = Color.bg_white("\t\t\t\t[PUBLISH_BROADCAST]") in
        Printf.sprintf "%s\n%s via topic %s:\n%s\n%s"
          (Color.bold (Color.red header))
          (Color.bold (Color.red header_msg))
          (Color.bold (Sexp.to_string_hum ~indent:1 (Types.sexp_of_topic topic)))
          (logger topic payload)
          (Color.bold (Color.red header)));

    publish_unicast = (fun node_id logger topic payload ->
        let width = Color.get_terminal_width () in
        let header = String.make width '-' in
        let header_msg = Color.bg_white( Printf.sprintf "\t\t\t\t[PUBLISH_UNICAST] Target Node ID = %d" node_id ) in
        Printf.sprintf "%s\n%s via topic %s:\n%s\n%s"
          (Color.bold (Color.magenta header))
          (Color.bold (Color.magenta header_msg))
          (Color.bold (Sexp.to_string_hum ~indent:1 (Types.sexp_of_topic topic)))
          (logger topic payload)
          (Color.bold (Color.magenta header))
      );

    subscribe = (fun sub_handle ->
        Color.bold (Color.green (Printf.sprintf "< +++ Subscribed +++ >: \n\ttopic=%s, node_id=%d, subscription_id=%d"
                                   (Sexp.to_string_hum ~indent:1 (Types.sexp_of_topic sub_handle.topic))
                                   sub_handle.node_id
                                   sub_handle.id))

      );

    unsubscribe = (fun sub_handle ->
        Color.bold (Color.red (Printf.sprintf "< --- Unsubscribed --- >: \n\ttopic=%s, node_id=%d, subscription_id=%d"
                                 (Sexp.to_string_hum ~indent:1 (Types.sexp_of_topic sub_handle.topic))
                                 sub_handle.node_id
                                 sub_handle.id))
      );

    enqueue = (fun topic queue_size ->
        let topic_str = Color.bold (Color.cyan (Sexp.to_string_hum ~indent:1 (Types.sexp_of_topic topic))) in
        Printf.sprintf "%s Enqueued message, queue size now %s"
          (Color.bold(Color.underline(Color.yellow "[ENQUEUE]")) )
          (Color.magenta (Int.to_string queue_size)) ^ " for topic " ^ topic_str
      );

    drain_start = (fun batch_size ->
        Color.underline(Printf.sprintf "%s Starting to drain queue of %s messages..."
          (Color.bold (Color.bright_blue "[DRAIN_START]"))
          (Color.bold (Color.magenta (Int.to_string batch_size))))
      );

    drain_end = (fun () ->
        Color.underline(Color.bold (Color.bright_blue "[DRAIN_END] Finished draining queue."))
      );

    print_stats = (fun () ->
        Color.bold (Color.blue "[STATS] Printing statistics...")
      );

  }

  let maybe_log logger_fn topic payload formatter =
    match logger_fn with
    | Some f -> Stdio.print_endline (formatter f topic payload)
    | None -> ()

  let format_and_log formatter content = Stdio.print_endline (formatter content)

  let create ?logger () =
    {next_id= 0; topics= Hashtbl.Poly.create (); queue= Queue.create (); logger}

  (** Returns the [topic_state] for [topic] if exists else initialises one for that topic and returns it.
      This allows lazy creation of topic entries.
  *)
  let ensure_topic_state t topic =
    match Hashtbl.find t.topics topic with
    | Some ts ->
        ts
    | None ->
        let ts =
          {subs= Hashtbl.Poly.create (); published= 0; delivered= 0; queued= 0}
        in
        Hashtbl.set t.topics ~key:topic ~data:ts ;
        ts

  let subscribe t ~topic ~node_id callback =
    let subscription_id = t.next_id in
    t.next_id <- subscription_id + 1 ;
    let ts = ensure_topic_state t topic in
    let sub_handle = {topic; id= subscription_id; node_id} in
    let subscription_info = {node_id; sub_handle; callback} in
    Hashtbl.add_exn ts.subs ~key:sub_handle ~data:subscription_info ;
    format_and_log formatters.subscribe sub_handle;
    sub_handle

  let unsubscribe t sub_handle =
    match Hashtbl.find t.topics sub_handle.topic with
    | None ->
        ()
    | Some ts ->
        Hashtbl.remove ts.subs sub_handle ;
        if
          Hashtbl.length ts.subs = 0
          && ts.published = 0 && ts.queued = 0 && ts.delivered = 0
        then Hashtbl.remove t.topics sub_handle.topic
        else Hashtbl.set t.topics ~key:sub_handle.topic ~data:ts

let publish_broadcast t ~topic payload =
  let ts = ensure_topic_state t topic in
  ts.published <- ts.published + 1;
  maybe_log t.logger topic payload formatters.publish_broadcast;
  if Hashtbl.is_empty ts.subs then ()
  else
    Hashtbl.iter ts.subs ~f:(fun subscription_info ->
        subscription_info.callback payload;
        ts.delivered <- ts.delivered + 1)


  let publish_unicast t ~topic ~node_id payload =
    match Hashtbl.find t.topics topic with
    | None -> ()
    | Some ts ->
      Hashtbl.iteri ts.subs ~f:(fun ~key ~data ->
          if key.node_id = node_id then
            data.callback payload;
          let formatter = formatters.publish_unicast node_id in
            maybe_log t.logger topic payload formatter
        );
       ts.delivered <- ts.delivered + 1

  let enqueue t thunk =
    let ((topic, _target_opt), _msg) = thunk in
    let ts = ensure_topic_state t topic in
    ts.queued <- ts.queued + 1 ;
    Queue.enqueue t.queue thunk;
    Stdio.print_endline (formatters.enqueue topic ts.queued)

  let drain t =
        (* Snapshot the current queue to isolate this batch *)
    let current_batch = Queue.to_list t.queue in
    Queue.clear t.queue ;
    let q_size = List.length current_batch in
    Stdio.print_endline
      ( "--- Draining the queue of " ^ Int.to_string q_size
        ^ " items from batch start" ) ;
    Stdio.print_endline (formatters.drain_start (q_size));
    List.iter current_batch ~f:(fun ((topic, node_id_opt), payload) ->
        match Hashtbl.find t.topics topic with
        | None ->
          ()
        | Some ts ->
          match node_id_opt with
          | None -> publish_broadcast t ~topic payload
          | Some node_id -> publish_unicast t  ~node_id ~topic payload;
            ts.queued <- Int.max 0 (ts.queued - 1));
    Stdio.print_endline (formatters.drain_end ())

  (* Any enqueued messages during publish will accumulate in t.queue for the next tick — not this one *)

  let stats t =
    Hashtbl.to_alist t.topics
    |> List.map ~f:(fun (topic, ts) ->
           ( topic
           , (Hashtbl.length ts.subs, ts.published, ts.delivered, ts.queued) ) )

  let print_stats t =
    Stdio.print_endline ( formatters.print_stats () );
    let stats = stats t in
    let header =
      " Topic                  | Subscribers | Published | Delivered | Queued "
    in
    let line = String.make (String.length header) '-' in
    Stdio.printf "\n%s\n%s\n%s\n" line header line ;
    List.iter stats ~f:(fun (topic, (subs, published, delivered, queued)) ->
        let topic_str = Sexp.to_string (Types.sexp_of_topic topic) in
        (* truncate or pad topic_str for aligned display *)
        let topic_str =
          if String.length topic_str > 22 then
            String.sub ~pos:0 ~len:19 topic_str ^ "..."
          else topic_str ^ String.make (22 - String.length topic_str) ' '
        in
        Stdio.printf " %s | %11d | %9d | %9d | %6d\n" topic_str subs published
          delivered queued ) ;
    Stdio.printf "%s\n" line
end

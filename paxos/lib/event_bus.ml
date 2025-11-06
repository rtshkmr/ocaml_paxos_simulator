[@@@ocaml.warning "-69"] (** TODO: remove unused variable warnings*)
open Base
open Types
open Log

(** Module type for a polymorphic type with a higher-kinded type parameter ['a t]
    ['a] is a higher-kinded type here. It needs to be fully applied for it to be used by a functor.
*)
module type S = sig
  type 'a t

  type sub_handle = {topic: Types.topic; id: int; node_id: Types.node_id}
  [@@deriving sexp, compare, equal, hash]

  type 'a serialiser = 'a -> string

  val create : payload_serialiser:'a serialiser -> unit -> 'a t

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

  type 'a serialiser = 'a -> string


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

  type 'a t =
    { mutable next_id: int
    ; topics: (Types.topic, 'a topic_state) Hashtbl.Poly.t
    ; queue: 'a enqueuable_thunk Queue.t
    ; logger: 'a Logger.t
    ; payload_serialiser: 'a serialiser;
    }

  let create ~payload_serialiser () =
    {next_id= 0; topics= Hashtbl.Poly.create (); queue= Queue.create (); logger=Logger.create(); payload_serialiser}

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
    Logger.log_subscribe t.logger topic node_id subscription_id;
    sub_handle

  let unsubscribe t sub_handle =
    match Hashtbl.find t.topics sub_handle.topic with
    | None ->
      ()
    | Some ts ->
      Hashtbl.remove ts.subs sub_handle ;
      Logger.log_unsubscribe t.logger sub_handle.topic sub_handle.node_id sub_handle.id ;
      if
        Hashtbl.length ts.subs = 0
        && ts.published = 0 && ts.queued = 0 && ts.delivered = 0
      then Hashtbl.remove t.topics sub_handle.topic
      else Hashtbl.set t.topics ~key:sub_handle.topic ~data:ts

  let publish_broadcast t ~topic payload =
    let ts = ensure_topic_state t topic in
    ts.published <- ts.published + 1;
    Logger.log_publish_broadcast t.logger topic ( payload |> t.payload_serialiser );

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
          if key.node_id = node_id then begin
            data.callback payload;
            Logger.log_publish_unicast t.logger node_id topic (payload |> t.payload_serialiser)
          end
        );
      ts.delivered <- ts.delivered + 1

  let enqueue t thunk =
    let ((topic, _target_opt), _msg) = thunk in
    let ts = ensure_topic_state t topic in
    ts.queued <- ts.queued + 1 ;
    Queue.enqueue t.queue thunk;
    Logger.log_enqueue t.logger topic ts.queued

  let drain t =
    (* Snapshot the current queue to isolate this batch *)
    let current_batch = Queue.to_list t.queue in
    Queue.clear t.queue ;
    let q_size = List.length current_batch in
    Stdio.print_endline
      ( "--- Draining the queue of " ^ Int.to_string q_size
        ^ " items from batch start" ) ;
    Logger.log_drain_start t.logger q_size;
    List.iter current_batch ~f:(fun ((topic, node_id_opt), payload) ->
        match Hashtbl.find t.topics topic with
        | None ->
          ()
        | Some ts ->
          match node_id_opt with
          | None -> publish_broadcast t ~topic payload
          | Some node_id -> publish_unicast t  ~node_id ~topic payload;
            ts.queued <- Int.max 0 (ts.queued - 1));

    Logger.log_drain_end t.logger

  (* Any enqueued messages during publish will accumulate in t.queue for the next tick — not this one *)

  let stats t =
    Hashtbl.to_alist t.topics
    |> List.map ~f:(fun (topic, ts) ->
        ( topic
        , (Hashtbl.length ts.subs, ts.published, ts.delivered, ts.queued) ) )

  let print_stats t =
    Logger.log_stats_header t.logger;
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

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

  type 'a bus_registrable_callback = 'a Message.Message.t -> unit

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

  val dump_stats : 'a t -> string
end

module Event_bus : S = struct
  (** sub_handle is the type for what a subscription handle looks like.
      -  [id] here refers to a subscription id (arbitrary for now)
  *)
  type sub_handle = {topic: Types.topic; id: int; node_id: Types.node_id}
  [@@deriving sexp, compare, equal, hash]

  type 'a callback = 'a -> unit

  (* TODO rename to payload_serialiser *)
  type 'a serialiser = 'a -> string

  type 'a bus_registrable_callback = 'a Message.Message.t -> unit

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
        ; id: int
    ; topics: (Types.topic, 'a topic_state) Hashtbl.Poly.t
    ; queue: 'a enqueuable_thunk Queue.t
    ; logger: Logger.t
    ; payload_serialiser: 'a serialiser;
    }

  let create ~payload_serialiser () =
    let random_id = Random.int 10000 in
    {id=random_id; next_id= 0; topics= Hashtbl.Poly.create (); queue= Queue.create (); logger=Logger.create Stdlib.__MODULE__ (); payload_serialiser}

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
    Logger.subscribe ~node_id t.logger ~bus_id:t.id ~topic_s:(topic |> Types.sexp_of_topic |> Sexp.to_string_hum) ~sub_id:subscription_id;
    sub_handle

  let unsubscribe ({id=bus_id;topics; logger; _}) ( {topic; node_id; id} as sub_handle ) =
    match topic |> Hashtbl.find topics with
    | None ->
      ()
    | Some ( {subs; published; queued; delivered} as ts ) ->
      Hashtbl.remove subs sub_handle ;
      Logger.unsubscribe ~node_id logger ~bus_id ~topic_s:(topic |> Types.sexp_of_topic |> Sexp.to_string_hum) ~sub_id:id ;
      if
        Hashtbl.length subs = 0
        && published = 0 && queued = 0 && delivered = 0
      then Hashtbl.remove topics topic
      else Hashtbl.set topics ~key:topic ~data:ts

  let publish_broadcast t ~topic payload =
    let ts = ensure_topic_state t topic in
    ts.published <- ts.published + 1;
    Logger.publish_broadcast ~bus_id:(t.id) t.logger ~topic_s:(topic |> Types.sexp_of_topic |> Sexp.to_string_hum) ~payload:( payload |> t.payload_serialiser );

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
            Logger.publish_unicast ~bus_id:(t.id) ~target_node:node_id t.logger ~topic_s:(topic |> Types.sexp_of_topic |> Sexp.to_string_hum)  ~payload:(payload |> t.payload_serialiser);
            data.callback payload
          end
        );
      ts.delivered <- ts.delivered + 1

  let enqueue t thunk =
    let ((topic, _target_opt), _msg) = thunk in
    let ts = ensure_topic_state t topic in
    ts.queued <- ts.queued + 1 ;
    Queue.enqueue t.queue thunk;
    (* BUG: (low priority: because we are snapshotting when draining then we aren't immediately clearing out the queue, the queue size here is not correct because the count includes the snapshot size) *)
    Logger.enqueue ~bus_id:t.id t.logger ~topic_s:(topic |> Types.sexp_of_topic |> Sexp.to_string_hum) ~queue_size:ts.queued

  let drain t =
    (* Snapshot the current queue to isolate this batch *)
    let current_batch = Queue.copy t.queue in
    Queue.clear t.queue ;
    let q_size = Queue.length current_batch in
    Logger.drain_start ~bus_id:t.id t.logger ~batch_size:q_size;
    while not (Queue.is_empty current_batch) do
      let ((topic, node_id_opt), payload) = Queue.dequeue_exn current_batch in
      match Hashtbl.find t.topics topic with
      | None -> ()
      | Some ts ->
          match node_id_opt with
          | None -> publish_broadcast t ~topic payload
          | Some node_id -> publish_unicast t ~node_id ~topic payload;
          ts.queued <- Int.max 0 (ts.queued - 1)
      done;
    Logger.drain_end ~bus_id:t.id t.logger

  let stats t =
    Hashtbl.to_alist t.topics
    |> List.map ~f:(fun (topic, ts) ->
        ( topic
        , (Hashtbl.length ts.subs, ts.published, ts.delivered, ts.queued) ) )

  let dump_stats t =
    let stats = stats t in
    let header = " Topic                  | Subscribers | Published | Delivered | Queued " in
    let line = String.make (String.length header) '-' in
    let buffer = Buffer.create 1024 in

    (* Append header section *)
    Buffer.add_string buffer ("\n" ^ line ^ "\n" ^ header ^ "\n" ^ line ^ "\n");

    (* Append each stat line *)
    List.iter stats ~f:(fun (topic, (subs, published, delivered, queued)) ->
        let topic_str = Sexp.to_string (Types.sexp_of_topic topic) in
        let topic_str =
          if String.length topic_str > 22 then
            String.sub topic_str ~pos:0 ~len:19 ^ "..."
          else topic_str ^ String.make (22 - String.length topic_str) ' '
        in
        Buffer.add_string buffer
          (Printf.sprintf " %s | %11d | %9d | %9d | %6d\n"
             topic_str subs published delivered queued));

    Buffer.add_string buffer (line ^ "\n");

    Buffer.contents buffer

  let print_stats t =
    let dump = dump_stats t in
    Logger.stats ~bus_id:t.id t.logger ~dump
end

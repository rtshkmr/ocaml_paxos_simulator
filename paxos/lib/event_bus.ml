[@@@ocaml.warning "-69"] (*TO DEPRECATE: sub_handle usage*)

open Base
open Types
open Log
open Message

module type S = sig
  type 'a t

  val id_of : _ t -> int

  type sub_handle = {topic: Types.topic; id: int; node_id: Types.node_id}
  [@@deriving sexp, compare, equal, hash]

  type 'a payload_serialiser = 'a -> string

  type 'a bus_registrable_callback = 'a Message.t -> unit

  val create : payload_to_string:'a payload_serialiser -> 'a t

  val subscribe :
       'a t
    -> topic:Types.topic
    -> node_id:Types.node_id
    -> node_alias:string
    -> ('a -> unit)
    -> sub_handle

  val unsubscribe : 'a t -> sub_handle:sub_handle -> alias:string -> unit

  val publish_broadcast : 'a t -> topic:Types.topic -> 'a -> unit

  val publish_unicast : 'a t -> topic:Types.topic -> node_id:int -> 'a -> unit

  type 'a enqueuable_thunk = (Types.topic * Types.node_id option) * 'a

  val enqueue : 'a t -> alias:string -> 'a enqueuable_thunk -> unit

  val drain : 'a t -> unit

  val stats : 'a t -> (Types.topic * (int * int * int * int)) list

  val display_stats : 'a t -> unit
end

module Event_bus : S = struct
  (**
  Polymorphic in-process pub/sub message broker for deterministic simulation.

  {b Design:}
  - Synchronous publish/subscribe for v0 (no concurrency yet for this version)
  - Type-generic: ['a t] carries messages of type ['a]
  - Subscription handles allow explicit unsubscribe
  - Buffered enqueue/drain pattern enables deterministic message delivery

  {b Typical usage:}
  {[
    (* Create a broker for string messages *)
    let bus = Event_bus.create ~payload_to_string:String.to_string in

    (* Subscribe a handler *)
    let handle = Event_bus.subscribe bus ~topic:MyTopic
                   ~node_id:1 ~node_alias:"alice"
                   (fun msg -> handle_message msg) in

    (* Enqueue messages (batched) *)
    Event_bus.enqueue bus ~alias:"alice" ((MyTopic, None), my_msg);

    (* Flush batch atomically *)
    Event_bus.drain bus
  ]}

  {b Future-proofing:}
  The synchronous API can be replaced with Lwt/Async later without
  changing client code, as long as drain becomes awaitable.
*)

  (** sub_handle is the type for what a subscription handle looks like.
      -  [id] here refers to a subscription id (arbitrary for now)
  *)
  type sub_handle = {topic: Types.topic; id: int; node_id: Types.node_id}
  [@@deriving sexp, compare, equal, hash]

  type 'a callback = 'a -> unit [@@deriving sexp]

  type 'a payload_serialiser = 'a -> string

  type 'a bus_registrable_callback = 'a Message.t -> unit

  type 'a subscription_info = {sub_handle: sub_handle; callback: 'a callback}

  type 'a topic_state =
    { subs: (sub_handle, 'a subscription_info) Hashtbl.t
    ; mutable published: int
    ; mutable delivered: int
    ; mutable queued: int }

  (** A thunk that can be queued such that it will either be published as a broadcast or as a publish to
      a single node *)
  type 'a enqueuable_thunk = (Types.topic * Types.node_id option) * 'a

  type 'a t =
    { mutable next_id: int
    ; id: int
    ; topics: (Types.topic, 'a topic_state) Hashtbl.Poly.t
    ; queue: 'a enqueuable_thunk Queue.t
    ; logger: Logger.t
    ; payload_to_string: 'a payload_serialiser }

  let id_of t = t.id

  let create ~payload_to_string =
    let random_id = Random.int 10000 in
    { id= random_id
    ; next_id= 0
    ; topics= Hashtbl.Poly.create ()
    ; queue= Queue.create ()
    ; logger= Logger.create Stdlib.__MODULE__ ()
    ; payload_to_string }

  (** Returns the [topic_state] for [topic] if exists else initialises one for that topic and returns it.
      This allows lazy creation of topic entries.
  *)
  let ensure_topic_state {topics; _} topic =
    match topic |> Hashtbl.find topics with
    | Some ts ->
        ts
    | None ->
        let ts =
          {subs= Hashtbl.Poly.create (); published= 0; delivered= 0; queued= 0}
        in
        topics |> Hashtbl.set ~key:topic ~data:ts ;
        ts

  let subscribe ({id; next_id= sub_id; logger; _} as t) ~topic ~node_id
      ~node_alias callback =
    t.next_id <- sub_id + 1 ;
    let {subs; _} = ensure_topic_state t topic in
    let sub_handle = {topic; id= sub_id; node_id} in
    let subscription_info = {sub_handle; callback} in
    Hashtbl.add_exn subs ~key:sub_handle ~data:subscription_info ;
    Logger.subscribe ~node_id ~node_alias logger ~bus_id:id
      ~topic_s:(topic |> Types.topic_to_str)
      ~sub_id ;
    sub_handle

  let unsubscribe {id= bus_id; topics; logger; _} ~sub_handle ~alias =
    let {topic; node_id; id} = sub_handle in
    match topic |> Hashtbl.find topics with
    | None ->
        ()
    | Some ({subs; published; queued; delivered} as ts) ->
        sub_handle |> Hashtbl.remove subs ;
        Logger.unsubscribe ~alias ~node_id logger ~bus_id
          ~topic_s:(topic |> Types.topic_to_str)
          ~sub_id:id ;
        if
          Hashtbl.length subs = 0
          && published = 0 && queued = 0 && delivered = 0
        then Hashtbl.remove topics topic
        else Hashtbl.set topics ~key:topic ~data:ts

  let publish_broadcast ({id; logger; payload_to_string; _} as t) ~topic payload
      =
    let ({published; subs; delivered; _} as ts) = ensure_topic_state t topic in
    ts.published <- published + 1 ;
    Logger.publish_broadcast ~bus_id:id logger
      ~topic_s:(topic |> Types.topic_to_str)
      ~payload:(payload |> payload_to_string) ;
    if Hashtbl.is_empty subs then ()
    else
      Hashtbl.iter subs ~f:(fun {callback; _} ->
          payload |> callback ;
          ts.delivered <- delivered + 1 )

  let publish_unicast {topics; id; payload_to_string; logger; _} ~topic ~node_id
      payload =
    match Hashtbl.find topics topic with
    | None ->
        ()
    | Some ({subs; delivered; _} as ts) ->
        let deliveries =
          Hashtbl.fold subs ~init:0 ~f:(fun ~key ~data acc ->
              if key.node_id = node_id then (
                Logger.publish_unicast ~bus_id:id ~target_node:node_id logger
                  ~topic_s:(topic |> Types.topic_to_str)
                  ~payload:(payload |> payload_to_string) ;
                payload |> data.callback ;
                acc + 1 )
              else acc )
        in
        ts.delivered <- delivered + deliveries

  let enqueue ({id; queue; logger; _} as t) ~alias thunk =
    let (topic, _target_opt), _msg = thunk in
    let ({queued; _} as ts) = ensure_topic_state t topic in
    ts.queued <- queued + 1 ;
    Queue.enqueue queue thunk ;
    Logger.enqueue ~bus_id:id logger
      ~topic_s:(topic |> Types.topic_to_str)
      ~queue_size:ts.queued ~alias

  let drain ({queue; id; logger; topics; _} as t) =
    (* Snapshot the current queue to isolate this batch *)
    let current_batch = Queue.copy queue in
    Queue.clear queue ;
    let q_size = Queue.length current_batch in
    Logger.drain_start ~bus_id:id logger ~batch_size:q_size ;
    while not (Queue.is_empty current_batch) do
      let (topic, node_id_opt), payload = Queue.dequeue_exn current_batch in
      match Hashtbl.find topics topic with
      | None ->
          ()
      | Some ts -> (
        match node_id_opt with
        | None ->
            publish_broadcast t ~topic payload
        | Some node_id ->
            publish_unicast t ~node_id ~topic payload ;
            ts.queued <- Int.max 0 (ts.queued - 1) )
    done ;
    Logger.drain_end ~bus_id:id ~batch_size:q_size logger

  let stats {topics; _} =
    Hashtbl.to_alist topics
    |> List.map ~f:(fun (topic, {subs; published; delivered; queued}) ->
        (topic, (Hashtbl.length subs, published, delivered, queued)) )

  let topic_stats {topics; _} =
    Hashtbl.to_alist topics
    |> List.map ~f:(fun (topic, {subs; published; delivered; queued}) ->
        let subscribers = Hashtbl.length subs in
        let topic_stat : Log_types.Log_event.topic_stat =
          {topic; subscribers; published; delivered; queued}
        in
        topic_stat )

  let display_stats t =
    Logger.inspect_bus_stats ~bus_id:t.id ~topic_stats:(topic_stats t) t.logger
end

(*
IMPROVEMENT CONSIDERATIONS
===========================
1. TODO [Defensive]:
   a) callback usage is not guarded from exns currently, so if we have any exn from callback usage, the whole bus will get killed. haha. Living life on the edge.
      I'm going to skip this for now because I can't find a clean way to define a safe_callback function without passing it a million params (this part smells)

      Possible inspiration:
      ```ocaml
      let safe_callback ~logger ~bus_id ~topic_s ~subscriber_info f payload =
        try
          f payload
        with ex ->
          Logger.callback_error
            ~bus_id
            ~topic_s
            ~subscriber_info
            ~exn:(Printexc.to_string ex)
            logger
      ```

2. Simpler performance improvements:
   a) avoiding queue copy if possible
   b) instead of using Hashtbl poly, we should use the specific hashtables

3. Tidying up:
   a) `sub_handle` is not being used  at the moment, consider deprecating
*)

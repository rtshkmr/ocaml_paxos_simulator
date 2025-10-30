open Base
open Types

(** Module type for a polymorphic type with a higher-kinded type parameter ['a t]
   ['a] is a higher-kinded type here. It needs to be fully applied for it to be used by a functor.
*)
module type S = sig
  type 'a t

  type sub_handle

  val create : ?logger:(Types.topic -> 'a -> string) -> unit -> 'a t

  val subscribe : 'a t -> topic:Types.topic -> ('a -> unit) -> sub_handle

  val unsubscribe : 'a t -> sub_handle -> unit

  val publish : 'a t -> topic:Types.topic -> 'a -> unit

  val enqueue : 'a t -> topic:Types.topic -> 'a -> unit

  val drain : 'a t -> unit

  val stats : 'a t -> (Types.topic * (int * int * int * int)) list
end

module Event_bus : S = struct
  type sub_handle = {topic: Types.topic; id: int}

  type 'a callback = 'a -> unit

  type 'a topic_state =
    { mutable subs: (int * 'a callback) list
    ; mutable published: int
    ; mutable delivered: int
    ; mutable queued: int }

  type 'a t =
    { mutable next_id: int
    ; topics: (Types.topic, 'a topic_state) Hashtbl.Poly.t
    ; queue: (Types.topic * 'a) Queue.t
    ; logger: (Types.topic -> 'a -> string) option }

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
        let ts = {subs= []; published= 0; delivered= 0; queued= 0} in
        Hashtbl.set t.topics ~key:topic ~data:ts ;
        ts

  let subscribe t ~topic cb =
    let id = t.next_id in
    t.next_id <- id + 1 ;
    let ts = ensure_topic_state t topic in
    ts.subs <- (id, cb) :: ts.subs ;
    {topic; id}

  let unsubscribe t handle =
    match Hashtbl.find t.topics handle.topic with
    | None ->
        ()
    | Some ts ->
        ts.subs <- List.filter ts.subs ~f:(fun (id, _) -> id <> handle.id) ;
        if
          List.is_empty ts.subs && ts.published = 0 && ts.queued = 0
          && ts.delivered = 0
        then Hashtbl.remove t.topics handle.topic
        else Hashtbl.set t.topics ~key:handle.topic ~data:ts

  let publish t ~topic payload =
    let ts = ensure_topic_state t topic in
    ts.published <- ts.published + 1 ;
    (* TODO: improve logger soon. Optionally log serialized payload *)
    ( match t.logger with
    | Some f ->
        ignore (f topic payload)
        (* logger side-effect only; caller's logger can persist it *)
    | None ->
        () ) ;
    match ts.subs with
    | [] ->
        ()
    | callbacks ->
        List.iter callbacks ~f:(fun (_, cb) ->
            cb payload ;
            ts.delivered <- ts.delivered + 1 )

  let enqueue t ~topic payload =
    let ts = ensure_topic_state t topic in
    ts.queued <- ts.queued + 1 ;
    Queue.enqueue t.queue (topic, payload)

  let drain t =
    let q_size = Queue.length t.queue in
    Stdio.print_endline
      ("Draining the queue of " ^ Int.to_string q_size ^ " items") ;
    while not (Queue.is_empty t.queue) do
      let topic, payload = Queue.dequeue_exn t.queue in
      (* adjust queued counter *)
      ( match Hashtbl.find t.topics topic with
      | None ->
          ()
      | Some ts ->
          ts.queued <- Int.max 0 (ts.queued - 1) ) ;
      (* publish synchronously *)
      publish t ~topic payload
    done

  let stats t =
    Hashtbl.to_alist t.topics
    |> List.map ~f:(fun (topic, ts) ->
           (topic, (List.length ts.subs, ts.published, ts.delivered, ts.queued)) )
end

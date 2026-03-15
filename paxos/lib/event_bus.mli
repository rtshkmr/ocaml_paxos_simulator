open Base
open Types
open Log_types

(** Polymorphic in-process pub/sub message broker for deterministic simulation.

    {b Design:}
    - Synchronous publish/subscribe for v0 (no concurrency)
    - Type-generic: ['a t] carries messages of type ['a]
    - Subscription handles allow explicit unsubscribe
    - Buffered enqueue/drain pattern enables deterministic message delivery

    {b Typical usage:}
    {[
      (* Create a broker for string messages *)
      let bus = Event_bus.create ~payload_to_string:String.to_string in

      (* Subscribe a handler *)
      let handle =
        Event_bus.subscribe bus ~topic:MyTopic ~node_id:1 ~node_alias:"alice"
          (fun msg -> handle_message msg)
      in

      (* Enqueue messages (batched) *)
      Event_bus.enqueue bus ~alias:"alice" ((MyTopic, None), my_msg);

      (* Flush batch atomically *)
      Event_bus.drain bus
    ]}

    {b Future-proofing:} The synchronous API can be replaced with Lwt/Async
    later without changing client code, as long as drain becomes awaitable. *)
module type S = sig
  type 'a t

  val id_of : _ t -> int
  val logger_of : _ t -> Log.Logger.t

  type sub_handle = { topic : Types.topic; id : int; node_id : Types.node_id }
  [@@deriving sexp, compare, equal, hash]

  type 'a payload_serialiser = 'a -> string

  type 'a bus_registrable_callback = 'a Message.Message.t -> unit
  (** a callback that we can use for communicating via the bus this works
      because the node would have been bound to the callback, event bus can
      remain passive about it. *)

  val create :
    ?log_level:Log_level.t ->
    payload_to_string:'a payload_serialiser ->
    unit ->
    'a t

  val subscribe :
    'a t ->
    topic:Types.topic ->
    node_id:Types.node_id ->
    node_alias:string ->
    ('a -> unit) ->
    sub_handle

  val unsubscribe : 'a t -> sub_handle:sub_handle -> alias:string -> unit
  val publish_broadcast : 'a t -> topic:Types.topic -> 'a -> unit
  val publish_unicast : 'a t -> topic:Types.topic -> node_id:int -> 'a -> unit

  type 'a enqueuable_thunk = (Types.topic * Types.node_id option) * 'a

  val enqueue : 'a t -> alias:string -> 'a enqueuable_thunk -> unit

  val drain : 'a t -> unit
  (** Atomically flush all enqueued messages and deliver to subscribers.

      {b Behavior:}
      - Snapshots current queue (avoids re-entrancy issues)
      - Clears queue before processing (new enqueues go to next tick)
      - Invokes publish_broadcast or publish_unicast per message
      - Decrements [topic_state.queued] counters

      {b Determinism:} Snapshot semantics ensure that messages enqueued
      {i during} drain are not delivered until the {i next} drain call. This
      prevents unbounded recursion and makes ticks well-defined.

      Example:
      {[
        Bus.enqueue bus ((Topic, None), msg1);
        Bus.enqueue bus ((Topic, None), msg2);
        Bus.drain bus
        (* Delivers msg1, msg2 *)
        (* If msg1's handler enqueued msg3, msg3 waits for next drain *)
      ]} *)

  val stats : 'a t -> (Types.topic * (int * int * int * int)) list
  val display_stats : 'a t -> unit
end

module Event_bus : S

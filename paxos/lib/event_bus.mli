open Base
open Types

module type S = sig
  type 'a t

  type sub_handle

  val create : ?logger:(Types.topic -> 'a -> string) -> unit -> 'a t
  (** Create a bus for payload type 'a.
      [?logger] is an optional function to serialize events for logging/visualisation:
        logger : topic -> payload -> string
  *)

  val subscribe : 'a t -> topic:Types.topic -> ('a -> unit) -> sub_handle
  (** Subscribe: returns a handle used for fine-grained unsubscribe.
      The callback will be invoked synchronously during publish/drain. *)

  val unsubscribe : 'a t -> sub_handle -> unit
  (** Unsubscribe using the handle returned earlier. *)

  val publish : 'a t -> topic:Types.topic -> 'a -> unit
  (** Synchronous, inline publish to all subscribers for the topic *)

  val enqueue : 'a t -> topic:Types.topic -> 'a -> unit
  (** Queue-based publish for deterministic simulation step. *)

  val drain : 'a t -> unit
  (** Drain the queue dispatching events FIFO. *)

  val stats : 'a t -> (Types.topic * (int * int * int * int)) list
  (** Stats: returns per-topic (subscribers_count, published_count, delivered_count, queued_count).
      this isn't that important, we can iterate on it some other time, it's basically simulation-level stats that the event_bus can give.
*)
  (*                 ^subs  ^published ^delivered ^queued *)
end

module Event_bus : S

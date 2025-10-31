open Base
open Types

module type S = sig
  type 'a t

  type sub_handle [@@deriving sexp, compare, equal, hash]

  val create : ?logger:(Types.topic -> 'a -> string) -> unit -> 'a t
  (** Create a bus for payload type 'a.
      [?logger] is an optional function to serialize events for logging/visualisation:
        logger : topic -> payload -> string
  *)

  val subscribe : 'a t -> topic:Types.topic -> ('a -> unit) -> sub_handle
  (** Subscribe: returns a handle used for fine-grained unsubscribe.
      The callback will be invoked synchronously during publish/drain.

      Subscription is an act of callback registration.
      We keep topics as a Hashtbl within a [topics] field and that Hashtbl is mutable.
      A [Types.topic] as key gives us the data which is a [topic_state] that is a record of a few mutable fields.

      [topic_state] is the mutable state that we are concerned with, and the subscriptions are kept within the [topic_state.subs] list.

      [Event_bus] can therefore do message passing by calling the callbacks.

      TODO: use a map instead of a list for [topic_state.subs]

*)

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

  val print_stats : 'a t -> unit
end

module Event_bus : S

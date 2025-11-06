open Base
open Types

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

  type 'a enqueuable_thunk = (Types.topic * Types.node_id option) * 'a

  val enqueue : 'a t -> 'a enqueuable_thunk -> unit

  val drain : 'a t -> unit

  val stats : 'a t -> (Types.topic * (int * int * int * int)) list

  val print_stats : 'a t -> unit

  val dump_stats : 'a t -> string
end

module Event_bus : S

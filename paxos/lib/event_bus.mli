open Base
open Types

module type S = sig
  type 'a t

  val id_of : _ t -> int

  type sub_handle = {topic: Types.topic; id: int; node_id: Types.node_id}
  [@@deriving sexp, compare, equal, hash]

  type 'a payload_serialiser = 'a -> string

  (** a callback that we can use for communicating via the bus
      this works because the node would have been bound to the callback, event bus can remain passive about it.
  *)
  type 'a bus_registrable_callback = 'a Message.Message.t -> unit

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

module Event_bus : S

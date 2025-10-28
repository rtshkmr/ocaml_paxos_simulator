open Base

(**
  Minimal in-process pub/sub broker signature.

  Intent:
  - Synchronous publish/subscribe API for v0 deterministic simulation.
  - Keep type generic so the same broker signature can be implemented later with Lwt/Async.
  - Subscriber callbacks run synchronously during publish (for the pure simulation).
*)
module type Broker = sig
  type 'a t
  type sub_handle (** self-referential handle*)

  val create : unit -> 'a t

  (** subscribe returns a handle that can be used to unsubscribe *)
  val subscribe : 'a t -> topic:string -> ('a -> unit) -> sub_handle
  val unsubscribe : 'a t -> sub_handle -> unit

  (** publish synchronously invokes subscriber callbacks *)
  val publish : 'a t -> topic:string -> 'a -> unit

  val stats : 'a t -> (string * int) list
end

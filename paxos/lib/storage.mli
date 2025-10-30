open Base

(**
  Abstract storage signature for acceptor persistence.

  Storage persists the latest promise or acceptance that underlies the current state.

  Intent:
  - Provide a minimal abstraction over persistence so acceptors can persist their
    promised/accepted records.
  - Keep API synchronous for v0; later the storage can be implemented with async IO
    but the core paxos logic will call the storage interface in the same places.

  Note:
  - The 'key' and 'value' are abstract; an implementation for acceptor state will
    concretize these types (e.g., string -> serialized bytes or a small record).
*)
module type S = sig
  type t [@@deriving sexp]

  (** typically the <node_id> / <slot_id>*)
  type key [@@deriving sexp]

  (** value will typically be a compact record*)
  type value [@@deriving sexp]

  val create : ?config:string -> unit -> t
  val persist : t -> key -> value -> (unit, Error.t) Result.t
  val load : t -> key -> (value option, Error.t) Result.t
  val snapshot : t -> (unit, Error.t) Result.t
end


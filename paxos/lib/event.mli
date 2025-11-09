open Base
open Time

(** Event definitions, layered for extensibility and logging.
    This is expected to be used for the simulation / logging more so than for message passing.
  *)
module EventMeta : sig
  type t = {timestamp: Time.t; id: int} [@@deriving sexp, compare, equal]
end

module BaseEvent : sig
  type 'payload t =
    { meta: EventMeta.t
    ; payload: 'payload
          (** We keep this abstract so that we can support cases such as a message being a payload here*)
    }
  [@@deriving sexp, compare, equal]
end

module Event : sig
  type 'payload t = Msg of 'payload BaseEvent.t | Sys of string
  [@@deriving sexp, compare, equal]
end

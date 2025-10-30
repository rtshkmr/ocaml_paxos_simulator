
(* module type Value = sig *)
(*   type t [@@deriving sexp, compare, equal, hash] *)
(*   val to_string : t -> string *)
(* end *)

(**
  Value is the payload that flows through messages.

  Signature for the generic replicated value used in Paxos.

  Intent:
  - Make the core algorithm generic over value `t`.
  - Require standard derives for easy debugging and ordering (where meaningful).
  - Provide to_string for logging.
*)
module type S = sig
  type t [@@deriving sexp, compare, equal, hash]
  val to_string : t -> string
end

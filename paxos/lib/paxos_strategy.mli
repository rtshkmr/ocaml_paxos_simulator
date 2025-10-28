(**
  Signature for Paxos algo variants (e.g. Classic, Multi-Paxos)
  This differs from PaxosPolicy which implements aspects of the
  Paxos spec that does not have any proper definition and is left to the implementation to figure out.

*)

open Base

module Paxos_strategy : sig
  type t =
    | Classic
    | Multi
    | Fast
  [@@deriving sexp, compare, equal]

  val description : t -> string
end


(* module type Paxos_strategy = sig *)
(*   type t *)
(*   type config *)

(*   val create : config -> t *)

(*   (\** choose backoff in milliseconds; implementation may consult internal state *\) *)
(*   val choose_backoff_ms : t -> int *)

(*   (\** optional leader hint (None if no hint) *\) *)
(*   val leader_hint : t -> Types.node_id option *)

(*   (\** called when proposer notices repeated collisions; allows strategy to adapt *\) *)
(*   val on_collision : t -> unit *)
(* end *)

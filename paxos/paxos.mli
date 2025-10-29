open Base

(** Defines operational policies governing runtime behavior.

    Intent:
    - Strategy modules encapsulate policies that Paxos implementations may vary:
        * How to choose backoff before retrying proposals
        * Whether to attempt leader hints / master lease behaviour
        * Tie-breaking and randomization policies
    - Keep the API minimal: small hooks used by the orchestrator / proposer.

    - future: perhaps I may be able to demonstrate runtime loading of policies via this demo repo.

*)
module Policy : sig
  type t =
    | Default
    | AntiInterference of {cooldown_ms: int}
    | Conservative
  [@@deriving sexp, compare, equal]

  val description : t -> string
end

(**
  Signature for Paxos algo variants (e.g. Classic, Multi-Paxos)
  This differs from Policy which implements aspects of the
  Paxos spec that does not have any proper definition and is left to the implementation to figure out.
*)
module Strategy : sig
  type t =
    | Classic
    | MultiPaxos of { window_size: int}
    | FastPaxos
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

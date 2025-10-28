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
module Paxos_policy : sig
  type t =
    | Default
    | AntiInterference
    | Conservative
  [@@deriving sexp, compare, equal]

  val description : t -> string
end

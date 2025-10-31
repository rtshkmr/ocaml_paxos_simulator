[@@@ocaml.warning "-26-32"]

open Paxos
open Base
module V = Value_string.Value_string

let my_value = V.t_of_sexp (Sexplib.Sexp.Atom "foo")

let my_2_val = V.t_of_sexp (Sexplib.Sexp.Atom "hell yesss")

module S = Storage_mem.Storage_mem (V)

let storage = S.create ()

module B = Event_bus.Event_bus

(* Improved logger: convert topics and messages to readable strings *)
let bus =
  B.create
    ~logger:(fun topic msg ->
      Printf.sprintf "[LOG][%s] %s"
        (Sexp.to_string (Types.Types.sexp_of_topic topic))
        (* Use a manual to_string *)
        (Sexplib.Sexp.to_string (Message.Message.sexp_of_t V.sexp_of_t msg)) )
    ()

module Node = Node.Make_node (V) (S) (B)

let n1 =
  Node.create ~id:1 ~roles:[Proposer] ~storage ~bus
    ~topics:[Types.Types.Coordination; Types.Types.Suggestion]
    ~state:Node.State.Echo ()

let n2 =
  Node.create ~id:2 ~roles:[Proposer] ~storage ~bus
    ~topics:[Types.Types.Coordination; Types.Types.Suggestion]
    ~state:Node.State.Echo ()

(* Proposal and simulation entrypoint *)
let () =
  let proposal = Types.Types.make_proposal_id ~seq:1 ~node:1 in
  Node.propose ~bus n1 ~proposal ~value:my_value ~quorum:(Some 2)

let () = B.print_stats bus

let () = B.drain bus

let () = B.print_stats bus

let () = B.drain bus

let () = B.print_stats bus

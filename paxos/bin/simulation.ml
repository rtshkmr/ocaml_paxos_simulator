[@@@ocaml.warning "-26"]

open Paxos
open Base

let () =
  let module V = Value_string.Value_string in
  let module S = Storage_mem.Storage_mem (V) in
  let storage = S.create () in
  let module B = Event_bus.Event_bus in
  let bus =
    Event_bus.Event_bus.create
      ~logger:(fun topic msg ->
        Printf.sprintf "[LOG][%s] %s"
          (Sexplib.Sexp.to_string (Types.Types.sexp_of_topic topic))
          (Sexplib.Sexp.to_string (Message.Message.sexp_of_t V.sexp_of_t msg)) )
      ()
  in
  let module Node = Node.Make_node (V) (S) (B) in
  let n1 = Node.create ~id:1 ~roles:[Proposer] ~storage ~bus () in
  let n2 = Node.create ~id:2 ~roles:[Acceptor] ~storage ~bus () in
  let proposal = Types.Types.make_proposal_id ~seq:1 ~node:1 in
  Node.propose ~bus n1 ~proposal ~value:(V.t_of_sexp (Sexplib.Sexp.Atom "foo")) ;
  Event_bus.Event_bus.drain bus

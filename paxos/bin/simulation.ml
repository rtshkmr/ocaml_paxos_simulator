open Paxos

(* --- we use this for the simulation harness! *)
let () = Stdio.print_endline Msg.greeting


(* module Topic = struct *)
(*   type t = Types.topic [@@deriving compare, sexp] *)
(* end *)

(* module Bus = Event_bus.Make(Topic) *)
(* module V = Value_string *)
(* module Storage = Storage_mem.Make_storage_mem(V) *)
(* module Node = Node.Make_node(V)(Storage)(Bus) *)

(* let () = *)
(*   let bus = Bus.create () in *)
(*   let s1 = Storage.create () in *)
(*   let s2 = Storage.create () in *)
(*   let n1 = Node.create ~id:"n1" ~roles:["Proposer"] ~storage:s1 in *)
(*   let n2 = Node.create ~id:"n2" ~roles:["Acceptor"] ~storage:s2 in *)
(*   Node.attach_handlers n2 ~bus ~topics:[Types.Coordination] ~handle_message:Node.handle_message; *)
(*   Node.propose n1 ~bus ~proposal:1 ~value:"hi there"; *)
(*   Stdio.printf "n2 state: %s\n" (Sexp.to_string_hum (Node.dump_state n2)); *)
(*   Node.shutdown n1 ~bus; *)
(*   Node.shutdown n2 ~bus; *)
(*   () *)

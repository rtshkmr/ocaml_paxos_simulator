[@@@ocaml.warning "-32"] (** TODO: remove unused variable warnings*)
open Message
open Simulator

module Simulation = struct
  (* module V = Value_string.Value_string *)
  module V = Simulator.V
  module B = Event_bus.Event_bus
  module S = Storage_mem.Storage_mem (V)
  module Node = Node.Make_node (V) (S) (B)

  let make_val string = V.t_of_sexp (Sexplib.Sexp.Atom string)
  (** can be coordinated, can be controlled by simulator*)
  let base_topics = [Types.Types.Coordination; Types.Types.Simulation_control]

  let node_spec_1 : Config.node_spec =
    { node_id= 1
    ; roles= ["Proposer"; "Acceptor"]
    ; initial_quorum= Some 2
    ; topics= base_topics
    ; initial_state= None
    ; storage_config= None }

  let node_spec_2 : Config.node_spec =
    { node_id= 2
    ; roles= ["Proposer"; "Acceptor"]
    ; initial_quorum= Some 2
    ; topics= base_topics
    ; initial_state= None
    ; storage_config= None }

  let node_spec_lists = [node_spec_1; node_spec_2]

  let sim_config : Config.t =
    { max_ticks= Some 100
    ; deterministic_seed= Some 181
    ; default_quorum= Some 3
    ; initial_nodes= node_spec_lists
    ; log_jsonl= None }

  let sim = Simulator.create ~config:sim_config

  let n1 = Simulator.add_node sim ~node_spec:node_spec_1

  let n2 = Simulator.add_node sim ~node_spec:node_spec_2

  (* let first_msg = *)
  (*   let proposal_id = Types.Types.make_proposal_id ~seq:1 ~node:1 in *)
  (*   let time = Simulator.current_time sim in *)
  (*   let from_id_val = 1 in *)
  (*   let coord_msg = Message.make_permission_request ~msg_id:1 ~time ~topic:Types.Types.Coordination ~from:from_id_val ~proposal:proposal_id ~value:(make_val "Let's go Ritesh, let's go !!!") in *)
  (*   Message.Coordination coord_msg *)

 let first_msg =
  let open Simulator in
  let proposal_id = Types.Types.make_proposal_id ~seq:1 ~node:1 in
  let time = current_time sim in
  let from_id_val = 1 in
  let coord_msg = Message.make_permission_request ~msg_id:1 ~time ~topic:Types.Types.Coordination ~from:from_id_val ~proposal:proposal_id ~value:(make_val "Let's go Ritesh Let's go!!!") in
  let raw_msg = Message.Coordination coord_msg in
  Simulator.msg_of_message raw_msg

  let run() =
    Simulator.print_bus_stats sim;
    Simulator.send_message sim ~topic:Types.Types.Coordination ~from:n1 ~to_:(Some n2) ~msg:first_msg;
    Simulator.print_bus_stats sim;
    Simulator.step sim;
    Simulator.print_bus_stats sim;



  (* let create_nodes ~bus = *)
  (*   let num_sim_nodes = 2 in *)
  (*   let base_config = *)
  (*     Node.make_config ~quorum:(Some num_sim_nodes) ~roles:[Proposer] *)
  (*   in *)
  (*   let n1_config = base_config ~storage:(S.create ()) in *)
  (*   let n2_config = base_config ~storage:(S.create ()) in *)
  (*   let n1 = *)
  (*     Node.create ~id:1 ~bus ~topics:base_topics ~config:n1_config *)
  (*       ~state:Node.State.Echo () *)
  (*   in *)
  (*   let n2 = *)
  (*     Node.create ~id:2 ~bus ~topics:base_topics ~config:n2_config *)
  (*       ~state:Node.State.Echo () *)
  (*   in *)
  (*   (n1, n2) *)

  (* let bus = *)
  (*   B.create *)
  (*     ~logger:(fun topic msg -> *)
  (*       Printf.sprintf "[LOG][%s] %s" *)
  (*         (Sexp.to_string (Types.Types.sexp_of_topic topic)) *)
  (*         (Sexplib.Sexp.to_string (Message.Message.sexp_of_t V.sexp_of_t msg)) ) *)
  (*     () *)

  (* let n1, n2 = create_nodes ~bus *)

  (* let clock = Time.create_clock () *)

  (* let run () = *)
  (*   (\* Example proposal *\) *)
  (*   let time = Time.now clock in *)
  (*   let proposal = Types.Types.make_proposal_id ~seq:1 ~node:1 in *)
  (*   Node.propose ~msg_id:1 ~time ~bus n1 ~proposal *)
  (*     ~value:(make_val "Let's go Ritesh, let's go !!!") ; *)
  (*   Time.tick clock ; *)
  (*   B.print_stats bus ; *)
  (*   B.drain bus ; *)
  (*   B.print_stats bus ; *)
  (*   B.drain bus ; *)
  (*   Stdio.print_endline "\n\nNow, we will just make node 2 idle" ; *)
  (*   Node.make_node_idle ~msg_id:2 ~time:2 ~bus n1 ~node_id:2 ; *)
  (*   B.print_stats bus ; *)
  (*   let time = Time.now clock in *)
  (*   let proposal = Types.Types.make_proposal_id ~seq:2 ~node:1 in *)
  (*   Node.propose ~msg_id:3 ~time ~bus n1 ~proposal *)
  (*     ~value:(make_val "Anyone out there?") ; *)
  (*   B.drain bus ; *)
  (*   B.print_stats bus *)
end

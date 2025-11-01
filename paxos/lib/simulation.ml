open Base

module Simulation = struct
  module V = Value_string.Value_string
  module B = Event_bus.Event_bus
  module S = Storage_mem.Storage_mem (V)
  module Node = Node.Make_node (V) (S) (B)

  let make_val string = V.t_of_sexp (Sexplib.Sexp.Atom string)

  (** can be coordinated, can be controlled by simulator*)
  let base_topics = [Types.Types.Coordination; Types.Types.Simulation_control]

  let create_nodes ~bus =
    let num_sim_nodes = 2 in
    let base_config =
      Node.make_config ~quorum:(Some num_sim_nodes) ~roles:[Proposer]
    in
    let n1_config = base_config ~storage:(S.create ()) in
    let n2_config = base_config ~storage:(S.create ()) in
    let n1 =
      Node.create ~id:1 ~bus ~topics:base_topics ~config:n1_config
        ~state:Node.State.Echo ()
    in
    let n2 =
      Node.create ~id:2 ~bus ~topics:base_topics ~config:n2_config
        ~state:Node.State.Echo ()
    in
    (n1, n2)

  let bus =
    B.create
      ~logger:(fun topic msg ->
        Printf.sprintf "[LOG][%s] %s"
          (Sexp.to_string (Types.Types.sexp_of_topic topic))
          (Sexplib.Sexp.to_string (Message.Message.sexp_of_t V.sexp_of_t msg)) )
      ()

  let n1, n2 = create_nodes ~bus

  let run () =
    (* Example proposal *)
    let proposal = Types.Types.make_proposal_id ~seq:1 ~node:1 in
    Node.propose ~msg_id:1 ~time:1 ~bus n1 ~proposal
      ~value:(make_val "Let's go Ritesh, let's go !!!") ;
    B.print_stats bus ;
    B.drain bus ;
    B.print_stats bus ;
    B.drain bus ;
    Stdio.print_endline "\n\nNow, we will just make node 2 idle" ;
    Node.make_node_idle ~msg_id:2 ~time:2 ~bus n1 ~node_id:2 ;
    B.print_stats bus ;
    let proposal = Types.Types.make_proposal_id ~seq:2 ~node:1 in
    Node.propose ~msg_id:3 ~time:2 ~bus n1 ~proposal
      ~value:(make_val "Anyone out there?") ;
    B.drain bus ;
    B.print_stats bus
end

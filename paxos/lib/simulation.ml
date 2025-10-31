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

  let run () =
    let bus =
      B.create
        ~logger:(fun topic msg ->
          Printf.sprintf "[LOG][%s] %s"
            (Sexp.to_string (Types.Types.sexp_of_topic topic))
            (Sexplib.Sexp.to_string (Message.Message.sexp_of_t V.sexp_of_t msg)) )
        ()
    in
    let n1, _n2 = create_nodes ~bus in
    (* Example proposal *)
    let proposal = Types.Types.make_proposal_id ~seq:1 ~node:1 in
    Node.propose ~bus n1 ~proposal
      ~value:(make_val "Let's go Ritesh, let's go !!!") ;
    B.print_stats bus ;
    B.drain bus ;
    B.print_stats bus ;
    B.drain bus ;
    B.print_stats bus
end

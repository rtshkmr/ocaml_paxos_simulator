open Base
open Message
open Simulator

module Simulation = struct
  module V = Simulator.V
  module B = Event_bus.Event_bus
  module S = Storage_mem.Storage_mem (V)
  module Node = Node.Make_node (V) (S) (B)

  let make_val s = V.t_of_sexp (Sexplib.Sexp.Atom s)

  let base_topics =
    [Types.Types.Coordination; Types.Types.Simulation_control; Types.Types.Time]

  let make_node_spec ?(roles = ["Proposer"; "Acceptor"])
      ?(initial_cluster_size = Some 2) ?(topics = base_topics) ~node_id () :
      Config.node_spec =
    { node_id
    ; roles
    ; initial_cluster_size
    ; topics
    ; initial_state= None
    ; storage_config= None }

  let node_spec_1 = make_node_spec ~node_id:1 ()

  let node_spec_2 = make_node_spec ~node_id:2 ()

  let sim_config =
    { Config.max_ticks= Some 100
    ; deterministic_seed= Some 181
    ; default_cluster_size= Some 3
    ; initial_nodes= [node_spec_1; node_spec_2]
    ; log_jsonl= None }

  let sim = Simulator.create ~config:sim_config

  let n1 = Simulator.add_node sim ~node_spec:node_spec_1

  let n2 = Simulator.add_node sim ~node_spec:node_spec_2

  let print_flush s =
    Stdio.print_endline s ;
    Out_channel.flush Stdio.stdout

  let make_sim_control_event ~(sim : Simulator.t) ~(dispatch_time : Time.Time.t)
      ~(to_node_id : Types.Types.node_id)
      ~(make_msg :
            msg_id:int
         -> time:Time.Time.t
         -> node_id:Types.Types.node_id
         -> V.t Message.simulation_control_message ) =
    let msg_id = Simulator.next_msg_id sim in
    let raw_msg = make_msg ~msg_id ~time:dispatch_time ~node_id:to_node_id in
    let msg = Simulator.msg_of_message (Message.Control raw_msg) in
    let topic = Types.Types.Simulation_control in
    let thunk = ((topic, Some to_node_id), msg) in
    Simulator.enqueue_thunk sim ~time:dispatch_time thunk

  let paxos_initiation_events =
    [ Simulator.make_node_proposal_event sim ~time:2 ~initiator:n1
        ~proposal:(Types.Types.make_proposal_id ~node:1 ~seq:1)
        ~value:
          (V.t_of_sexp
             (Sexplib.Sexp.Atom
                "Let's go ritesh let's gooooooooo, much better feel" ) )
    ; Simulator.make_node_proposal_event sim ~time:10 ~initiator:n2
        ~proposal:(Types.Types.make_proposal_id ~node:2 ~seq:2)
        ~value:(V.t_of_sexp (Sexplib.Sexp.Atom "ANother one, let's gooo")) ]

  let print_events =
    [ Simulator.make_event sim ~time:1
        (fun () -> Simulator.print_bus_stats sim)
        ()
    ; Simulator.make_event sim ~time:2
        (fun () -> Simulator.print_bus_stats sim)
        ()
    ; Simulator.make_event sim ~time:5
        (fun () -> Simulator.print_bus_stats sim)
        ()
    ; Simulator.make_event sim ~time:6
        (fun () -> Simulator.print_bus_stats sim)
        () ]

  let sim_ctrl_events =
    [ make_sim_control_event ~sim ~dispatch_time:3 ~to_node_id:1
        ~make_msg:Message.make_sim_control_inactive_node
    ; make_sim_control_event ~sim ~dispatch_time:5 ~to_node_id:1
        ~make_msg:Message.make_sim_control_idle_node ]

  (* let predetermined_events = permission_events @ print_events @ sim_ctrl_events *)
  let predetermined_events =
    paxos_initiation_events @ print_events (*@ sim_ctrl_events *)

  let seed_predetermined_events sim events =
    List.iter ~f:(fun e -> e |> Simulator.schedule_event sim) events

  let rec run_with_pause sim =
    match In_channel.input_char In_channel.stdin with
    | Some ' ' ->
        print_flush "Simulation paused. Press 'r' to resume and 'q' to quit." ;
        let rec wait_resume () =
          match In_channel.input_char In_channel.stdin with
          | Some 'r' ->
              print_flush "Resuming simulation." ;
              run_with_pause sim
          | Some 'q' ->
              print_flush "Quitting simulation." ;
              Simulator.print_bus_stats sim
          | _ ->
              wait_resume ()
        in
        wait_resume ()
    | Some 'q' ->
        print_flush "Quitting simulation." ;
        Simulator.print_bus_stats sim
    | Some _ ->
        Simulator.step sim ; run_with_pause sim
    | None ->
        print_flush "EOF received. Quitting." ;
        Simulator.print_bus_stats sim

  let run () =
    seed_predetermined_events sim predetermined_events ;
    run_with_pause sim ;
    Simulator.print_bus_stats sim
end

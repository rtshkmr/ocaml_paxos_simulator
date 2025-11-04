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

  [@@@ocaml.warning "-27"] (* TODO fixme: remove this eventually @ cleanup *)

  let permission_event_factory my_string : Simulator.msg_factory =
   fun ~msg_id ~from ?to_node ~time () ->
    let proposal = Types.Types.make_proposal_id ~seq:0 ~node:1 in
    let from_id = Simulator.id_of_node from in
    let raw_msg =
      Message.make_permission_request ~msg_id ~time
        ~topic:Types.Types.Coordination ~from:from_id ~proposal
        ~value:(make_val my_string)
    in
    Simulator.msg_of_message (Message.Coordination raw_msg)

  let permission_events =
    [ Simulator.create_message_event sim ~time:2 ~topic:Types.Types.Coordination
        ~from:n1 ~to_node:n2
        ~msg_factory:(permission_event_factory "Let's go ritesh let's go")
        ()
    ; Simulator.create_message_event sim ~time:4 ~topic:Types.Types.Coordination
        ~from:n2
        ~msg_factory:(permission_event_factory "We are so close to our goal")
        ()
    ; Simulator.create_message_event sim ~time:4 ~topic:Types.Types.Coordination
        ~from:n2
        ~msg_factory:(permission_event_factory "We must persist")
        () ]

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

  let predetermined_events = permission_events @ print_events

  let seed_predetermined_events sim events =
    List.iter ~f:(fun e -> Simulator.schedule_event sim e) events

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

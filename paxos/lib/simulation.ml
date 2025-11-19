[@@@ocaml.warning "-27"]

open Base
open Simulator
open Log

module Simulation = struct
  let print_flush s =
    Stdio.print_endline s ;
    Out_channel.flush Stdio.stdout

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

  let run ?(scenario = "basic") ?(max_log_level = 1) ?(allow_step = true) () =
    let open Simulator in
    let open Simulation_loader in
    let {preamble; simulator; nodes; events} =
      scenario
      |> Printf.sprintf "data/%s_scenario.json"
      |> load_simulation_config_from_file
    in
    let logger = Logger.create Stdlib.__MODULE__ () in
    let sim = simulator |> of_spec in
    logger |> Logger.display_scenario_preamble ~scenario ~preamble ;
    nodes |> List.iter ~f:(fun n -> n |> hydrate_node sim |> seed_node_exn sim) ;
    events |> List.iter ~f:(fun e -> e |> hydrate_event sim |> seed_event sim) ;
    run_with_pause sim
end

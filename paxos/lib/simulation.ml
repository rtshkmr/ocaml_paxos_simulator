[@@@ocaml.warning "-27"]

open Base
open Simulator
open Ansi.Formatter

module Simulation = struct
  let print_flush s =
    Stdio.print_endline s ;
    Out_channel.flush Stdio.stdout

  let format_preamble preamble =
    let desc = preamble |> italic |> bright_yellow in
    let fence = "\t" ^ String.make 40 '%' |> bright_blue |> bold in
    Printf.sprintf "\n\n%s\n%s\n%s\n\n" fence desc fence

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
    let ({preamble; simulator; nodes; events} : Config.simulation_config) =
      scenario
      |> Printf.sprintf "data/%s_scenario.json"
      |> Simulation_loader.load_simulation_config_from_file
    in
    let sim = simulator |> Simulator.of_spec in
    nodes
    |> List.iter ~f:(fun node_spec ->
           ignore (Simulator.add_node_to_sim sim node_spec) ) ;
    events
    |> List.map ~f:(Simulator.hydrate_event sim)
    |> Simulator.seed_events sim ;
    preamble |> format_preamble |> Stdio.print_endline ;
    run_with_pause sim
end

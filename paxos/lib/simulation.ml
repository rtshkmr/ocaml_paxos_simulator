[@@@ocaml.warning "-27"]

open Base
open Simulator
open Log

module Simulation = struct
  (* TODO [LOG] shift to log statement so that we can have single source of formatting *)
  let print_flush s =
    Stdio.print_endline s ;
    Out_channel.flush Stdio.stdout

  let on_slash_command sim cmd =
    match String.strip cmd with
    | "" ->
        print_flush "Empty command."
    | cmd ->
        print_flush ("Handling command: " ^ cmd) ;
        (* TODO: [quality][sim] add in slash commands for dumping state of differen things
           1. dump node state
           2. dump partition state
           3. dump event bus state

           this will actually make it more investigative and fun to use.
        *)
        ()

  (* FIXME: [BUG] the pausing is happening but the execution of the command isn't really happening. Likely something minor so leaving it as a FIXME for now. *)
  let rec handle_paused_action sim =
    match In_channel.input_line In_channel.stdin with
    | None ->
        print_flush "EOF received." ;
        Simulator.print_bus_stats sim
    | Some line -> (
      match String.strip line with
      | "r" ->
          print_flush "Resuming simulation."
      | "q" ->
          print_flush "Quitting simulation." ;
          Simulator.print_bus_stats sim
      | cmd when String.is_prefix cmd ~prefix:"/" ->
          String.drop_prefix cmd 1 |> on_slash_command sim ;
          sim |> handle_paused_action
      | "" ->
          print_flush "No command entered. (r, q, /cmd)" ;
          sim |> handle_paused_action
      | other ->
          print_flush
            ("Unknown command: " ^ other ^ ". Use r=resume, q=quit, /<cmd>...") ;
          sim |> handle_paused_action )

  let handle_command sim =
    match In_channel.input_char In_channel.stdin with
    | Some 'q' ->
        print_flush "Quitting simulation." ;
        Simulator.print_bus_stats sim
    | Some ' ' ->
        print_flush "Paused. (r=resume, q=quit, /<cmd>)" ;
        sim |> handle_paused_action
    | Some '/' ->
        In_channel.input_line In_channel.stdin
        |> Option.value ~default:"" |> on_slash_command sim
    | Some _ ->
        () (* continue *)
    | None ->
        print_flush "EOF received. Quitting." ;
        Simulator.print_bus_stats sim

  let rec run_simulation sim ~allow_step =
    match (sim |> Simulator.is_runnable, allow_step) with
    | false, _ ->
        print_flush "Simulation is complete." ;
        Simulator.print_bus_stats sim
    | true, false ->
        (* "headless", complete-run *)
        "[headless]" |> print_flush ;
        sim |> Simulator.step ;
        sim |> run_simulation ~allow_step
    | true, true ->
        (* REPL-mode *)
        "[REPL-mode]" |> print_flush ;
        sim |> Simulator.step ;
        print_flush
          "Sim is paused at this tick.\n\
           Press any key to continue, or options: space=pause, q=quit, \
           /<cmd>..." ;
        sim |> handle_command ;
        sim |> run_simulation ~allow_step

  let run ?(max_log_level = 1) ?(allow_step = true) scenario_path =
    "[simulation::run]" |> print_flush ;
    let open Simulator in
    let open Simulation_loader in
    let {scenario_name; preamble; simulator; nodes; events} =
      scenario_path |> load_simulation_config_from_file
    in
    let logger = Logger.create Stdlib.__MODULE__ () in
    let sim = simulator |> of_spec in
    logger |> Logger.display_scenario_preamble ~scenario_name ~preamble ;
    (* TODO [LOG] the global log level injection can be done here, we can inject it into the hydration functions *)
    nodes |> seed_nodes_from_specs sim ;
    events |> seed_events_from_specs sim ;
    run_simulation sim ~allow_step
end

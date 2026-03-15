open Base
open Simulator
open Log
open Log_types

module Simulation = struct
  let print_flush s =
    Stdio.print_endline s ;
    Out_channel.flush Stdio.stdout

  let fini _sim =
    print_flush "🌙 Simulation shutting down... thanks for playing!" ;
    Stdlib.exit 0

  let repl_prompt =
    let open Ansi.Formatter in
    ">>>" |> bright_green |> bold |> italic

  let prompt_input () =
    Stdio.printf
      "Sim is paused at this tick.\n\
       Press any key to continue, or try: space=pause, r=resume, q=quit, /help \
       =get help\n\n\
       %s %!"
      repl_prompt

  let rec handle_paused_action sim =
    prompt_input () ;
    match In_channel.input_line In_channel.stdin with
    | None ->
        print_flush "EOF received."
    | Some line -> (
      match String.strip line with
      | "r" ->
          print_flush "Resuming simulation."
      | "q" ->
          fini sim
      | cmd when String.is_prefix cmd ~prefix:"/" ->
          Simulator.handle_slash_command sim cmd ;
          sim |> handle_paused_action
      | "" ->
          print_flush "No command entered. (r, q, /cmd)" ;
          sim |> handle_paused_action
      | other ->
          print_flush
            ( "Unknown command: " ^ other
            ^ ". Use r=resume, q=quit, /help or /<cmd>..." ) ;
          sim |> handle_paused_action )

  let handle_command sim =
    match In_channel.input_char In_channel.stdin with
    | Some 'q' ->
        fini sim
    | Some ' ' ->
        print_flush "Paused. (r=resume, q=quit, /<cmd>)" ;
        sim |> handle_paused_action
    | Some '/' ->
        In_channel.input_line In_channel.stdin
        |> Option.value ~default:""
        |> Simulator.handle_slash_command sim ;
        sim |> handle_paused_action
    | Some _ ->
        () (* continue *)
    | None ->
        print_flush "EOF received. Quitting."

  let with_runnable_sim sim ~f =
    if Simulator.is_runnable sim then f ()
    else print_flush "Simulation is complete." ;
    fini sim

  let rec run_simulation sim ~allow_step =
    with_runnable_sim sim ~f:(fun () ->
        Simulator.step sim ;
        match allow_step with
        | false ->
            run_simulation sim ~allow_step
        | true ->
            prompt_input () ;
            handle_command sim ;
            run_simulation sim ~allow_step )

  let run ?(override_log_level = Log_level.Info) ?(allow_step = true)
      scenario_path =
    let open Simulator in
    let open Simulation_loader in
    let {scenario_name; preamble; simulator; nodes; events} =
      scenario_path |> load_simulation_config_from_file
    in
    let sim = simulator |> of_spec ~override_log_level in
    sim |> logger_of
    |> Logger.display_scenario_preamble ~scenario_name ~preamble ;
    nodes |> seed_nodes_from_specs sim ;
    events |> seed_events_from_specs sim ;
    run_simulation sim ~allow_step
end

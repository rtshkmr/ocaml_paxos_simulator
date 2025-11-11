open Core
open Command.Let_syntax
open Color

let scenario_arg =
  Command.Arg_type.create (function
    | "basic" ->
        "basic"
    | "office_bakeoff" ->
        "office_bakeoff"
    | "parliament" ->
        "parliament"
    | s ->
        failwithf "Unknown scenario: %s" s () )

let desc_of_scenario scenario =
  let fence = "\t" ^ String.make 40 '%' |> Color.bright_blue |> Color.bold in
  let desc =
    match scenario with
    | "basic" ->
        "This is a basic paxos scenario.. TODO" |> Color.bright_yellow
    | "office_bakeoff" ->
        "This is a office bakeoff paxos scenario.. TODO" |> Color.bright_yellow
    | "parliament" ->
        "This is a parliament paxos scenario.. TODO" |> Color.bright_yellow
    | s ->
        failwithf "Unknown scenario: %s" s ()
  in
  Printf.sprintf "%s\n%s\n%s" fence desc fence

let log_level_arg =
  Command.Arg_type.create (function
    | "debug" ->
        0
    | "info" ->
        1
    | "warn" ->
        2
    | "error" ->
        3
    | s ->
        failwithf "Unknown log level: %s" s () )

let string_of_log_level = function
  | 0 ->
      "debug"
  | 1 ->
      "info"
  | 2 ->
      "warn"
  | 3 ->
      "error"
  | n ->
      failwithf "Unknown log level: %d" n ()

let describe_simulation_settings ~scenario ~max_log_level ~allow_step =
  let cli_tag = "[CLI]" |> Color.bright_yellow |> Color.bold in
  Printf.sprintf "%s: running scenario=%s, max_log_level=%s, allow_step=%b\n\n"
    cli_tag (scenario |> Color.bold)
    (max_log_level |> string_of_log_level |> Color.bold)
    allow_step
  |> Stdio.print_endline

let command_simulate =
  let welcome =
    "Welcome to our OCaml Paxos demo!" |> Color.bold |> Color.bright_magenta
  in
  let curr_action_msg =
    "For now, please choose a scenario and runtime options from below:"
    |> Color.italic |> Color.underline
  in
  let summary =
    welcome
    ^ "\n\n\
       Please use this paxos simulator by setting the following arguments.\n"
    ^ "These are just the predefined scenarios to watch, we could add in our \
       own custom scenarios as well!\n\n" ^ curr_action_msg
  in
  Command.basic ~summary
    [%map_open
      let scenario =
        flag "-scenario"
          (optional_with_default "basic" scenario_arg)
          ~doc:"SCENARIO choose (basic | office_bakeoff | parliament)"
      and max_log_level =
        flag "-max-log-level"
          (optional_with_default 1 log_level_arg)
          ~doc:"LEVEL logging threshold (debug|info|warn|error). Default=info"
      and allow_step =
        flag "-allow-step"
          (optional_with_default true bool)
          ~doc:"BOOL whether to allow stepping interaction (default: true)"
      in
      fun () ->
        describe_simulation_settings ~scenario ~max_log_level ~allow_step ;
        Simulation.Simulation.run ~scenario ~max_log_level ~allow_step ()]

let () =
  Command_unix.run ~version:"0.1"
    (Command.group ~summary:"Paxos sim" [("run", command_simulate)])

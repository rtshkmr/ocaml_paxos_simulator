open Core
open Command.Let_syntax
open Ansi.Formatter

module Scenario = struct
  type t = Basic | Office_bakeoff | Parliament
  [@@deriving equal, enumerate, sexp]

  let to_string = function
    | Basic ->
        "basic"
    | Office_bakeoff ->
        "office_bakeoff"
    | Parliament ->
        "parliament"

  let arg_type =
    let alist = List.map all ~f:(fun sc -> (to_string sc, sc)) in
    Command.Arg_type.of_alist_exn alist

  let flag = "-scenario"

  let doc = "SCENARIO (basic | office_bakeoff | parliament)"
end

module Log_level = struct
  type t = Debug | Info | Warn | Error [@@deriving sexp, equal, enumerate]

  let to_int = function Debug -> 0 | Info -> 1 | Warn -> 2 | Error -> 3

  let of_int_exn = function
    | 0 ->
        Debug
    | 1 ->
        Info
    | 2 ->
        Warn
    | 3 ->
        Error
    | n ->
        failwithf "Unknown log level: %d" n ()

  let to_string = function
    | Debug ->
        "debug"
    | Info ->
        "info"
    | Warn ->
        "warn"
    | Error ->
        "error"

  let arg_type =
    Command.Arg_type.of_alist_exn
      [("debug", Debug); ("info", Info); ("warn", Warn); ("error", Error)]

  let flag = "-max-log-level"

  let doc = "LEVEL (debug|info|warn|error). Default=info"
end

let describe_simulation_settings ~scenario ~max_log_level ~allow_step =
  let cli_tag = "[CLI]" |> bright_yellow |> bold in
  Printf.printf "%s: running scenario=%s, max_log_level=%s, allow_step=%b\n\n"
    cli_tag
    (Scenario.to_string scenario |> bold)
    (Log_level.to_string max_log_level |> bold)
    allow_step

let command_simulate =
  let summary =
    let welcome =
      "Welcome to our OCaml Paxos demo!" |> bold |> bright_magenta
    in
    let curr =
      "For now, please choose a scenario and runtime options from below:"
      |> italic |> underline
    in
    [%string
      "%{welcome}\n\n\
       Please use this paxos simulator by choosing arguments.\n\n\
       %{curr}"]
  in
  Command.basic ~summary
    [%map_open
      let scenario =
        flag Scenario.flag
          (optional_with_default Scenario.Basic Scenario.arg_type)
          ~doc:Scenario.doc
      and max_log_level =
        flag Log_level.flag
          (optional_with_default Log_level.Info Log_level.arg_type)
          ~doc:Log_level.doc
      and allow_step =
        flag "-allow-step"
          (optional_with_default true bool)
          ~doc:"BOOL whether to allow interaction steps"
      in
      fun () ->
        describe_simulation_settings ~scenario ~max_log_level ~allow_step ;
        Simulation.Simulation.run
          ~scenario:(Scenario.to_string scenario)
          ~max_log_level:(Log_level.to_int max_log_level)
          ~allow_step ()]

let () =
  Command_unix.run
    (Command.group ~summary:"Paxos sim" [("run", command_simulate)])

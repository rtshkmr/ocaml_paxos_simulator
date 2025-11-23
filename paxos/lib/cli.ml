open Core
open Command.Let_syntax
open Ansi.Formatter
open Log

module Scenario = struct
  type kind = Basic | Office_bakeoff | Parliament | Custom
  [@@deriving equal, enumerate, sexp]

  type t = {kind: kind; path: string} [@@deriving sexp]

  let to_string ({kind; path} : t) =
    match kind with
    | Basic ->
        "basic"
    | Office_bakeoff ->
        "office_bakeoff"
    | Parliament ->
        "parliament"
    | Custom ->
        "custom loaded from " ^ path

  let arg_type =
    Command.Arg_type.of_alist_exn
      [ ("basic", Basic)
      ; ("office_bakeoff", Office_bakeoff)
      ; ("parliament", Parliament)
      ; ("custom", Custom) ]

  let resolve_scenario_file = function
    | Basic, _ ->
        {kind= Basic; path= "data/basic_scenario.json"}
    | Office_bakeoff, _ ->
        {kind= Office_bakeoff; path= "data/office_bakeoff_scenario.json"}
    | Parliament, _ ->
        {kind= Parliament; path= "data/parliament_scenario.json"}
    | Custom, Some path ->
        {kind= Custom; path}
    | _ ->
        failwith "Can't resolve scenario file."

  let flag = "-scenario"

  let doc = "SCENARIO (basic | office_bakeoff | parliament | custom)"
end

let command_simulate =
  let summary =
    (* TODO [LOG] use ansi formatter here *)
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
      let scenario_kind =
        flag Scenario.flag
          (optional_with_default Scenario.Basic Scenario.arg_type)
          ~doc:Scenario.doc
      and scenario_file =
        flag "-scenario-file" (optional string)
          ~doc:
            "PATH path to custom scenario json (required if -scenario custom)"
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
        let scenario =
          Scenario.resolve_scenario_file (scenario_kind, scenario_file)
        in
        scenario.path
        |> Simulation.Simulation.run
             ~max_log_level:(Log_level.to_int max_log_level)
             ~allow_step]

let () =
  Command_unix.run
    (Command.group ~summary:"Paxos sim" [("run", command_simulate)])

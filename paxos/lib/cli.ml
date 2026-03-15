open Core
open Command.Let_syntax
open Ansi.Formatter
open Log_types

module Scenario = struct
  type kind =
    | Basic
    | Office_bakeoff
    | Camel_caravan_basic
    | Camel_caravan_complex
    | Parliament_basic
    | Parliament_complex
    | Custom
  [@@deriving equal, enumerate, sexp]

  type t = { kind : kind; path : string } [@@deriving sexp]

  let arg_type =
    Command.Arg_type.of_alist_exn
      [
        ("basic", Basic);
        ("office_bakeoff", Office_bakeoff);
        ("camel_caravan", Camel_caravan_basic);
        ("camel_caravan_complex", Camel_caravan_complex);
        ("parliament", Parliament_basic);
        ("parliament_complex", Parliament_complex);
        ("custom", Custom);
      ]

  let scenario_dir = "data/scenarios/"

  let resolve_scenario_file = function
    | Basic, _ -> { kind = Basic; path = scenario_dir ^ "basic_scenario.json" }
    | Office_bakeoff, _ ->
        {
          kind = Office_bakeoff;
          path = scenario_dir ^ "office_bakeoff_scenario.json";
        }
    | Camel_caravan_basic, _ ->
        {
          kind = Camel_caravan_basic;
          path = scenario_dir ^ "caravan_scenario_basic.json";
        }
    | Camel_caravan_complex, _ ->
        {
          kind = Camel_caravan_complex;
          path = scenario_dir ^ "caravan_scenario_complex.json";
        }
    | Parliament_basic, _ ->
        {
          kind = Parliament_basic;
          path = scenario_dir ^ "parliament_scenario_basic.json";
        }
    | Parliament_complex, _ ->
        {
          kind = Parliament_complex;
          path = scenario_dir ^ "parliament_scenario_complex.json";
        }
    | Custom, Some path -> { kind = Custom; path }
    | _ -> failwith "Can't resolve scenario arguments."

  let flag = "-scenario"
  let doc = "SCENARIO Select the predefined scenario to simulate."
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
            "PATH The file path to custom scenario file .json. This is \
             required if -scenario custom"
      and override_log_level =
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
        |> Simulation.Simulation.run ~override_log_level ~allow_step]

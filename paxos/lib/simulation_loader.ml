open Base
open Simulator
open Sim_event

type simulation_config =
  { scenario_name: string
  ; preamble: string
  ; simulator: Simulator.spec
  ; nodes: Simulator.N.spec list
  ; events: Sim_event.spec list }
[@@deriving yojson]

let load_simulation_config_from_file file_path =
  (* TODO: [LOG] extract this to a logger function or something *)
  Stdio.printf "[Simulation_loader]: loading the simulation from %s\n%!"
    file_path ;
  match file_path |> Yojson.Safe.from_file |> simulation_config_of_yojson with
  | Ok config ->
      config
  | Error e ->
      failwith ("Failed to parse simulation config: " ^ e)

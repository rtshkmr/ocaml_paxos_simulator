open Base
open Config

let load_simulation_config_from_file file_path =
  Stdio.printf "[Simulation_loader]: loading the simulation from %s\n%!"
    file_path ;
  let parsed =
    file_path |> Yojson.Safe.from_file |> simulation_config_of_yojson
  in
  match parsed with
  | Ok config ->
      config
  | Error e ->
      failwith ("Failed to parse simulation config: " ^ e)

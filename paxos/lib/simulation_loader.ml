open Base
open Config
open Sim_event
open Simulator
open Types
module V = Simulator.V
module NodeImpl = Simulator.NodeImpl

(* convenience routine for converting string to V.t *)
let make_val s = V.t_of_sexp (Sexplib.Sexp.Atom s)

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

let hydrate_proposal_event sim ({id; target; data; time; _} : Sim_event.spec) =
  match (target, data) with
  | Some alias, Some value_str -> (
    match Simulator.get_node_by_alias sim alias with
    | Some node ->
        let assertion =
          { Types.proposal=
              Types.make_proposal_id ~node:(NodeImpl.id_of node) ~seq:1
          ; value= make_val value_str }
        in
        let action () =
          let msg_id = sim |> Simulator.next_msg_id in
          NodeImpl.propose node ~msg_id ~time ~bus:Simulator.bus ~assertion
        in
        { Sim_event.id= Option.value id ~default:(Simulator.next_event_id sim)
        ; time
        ; kind= Sim_event.Custom "proposal"
        ; action }
    | None ->
        failwith ("Unknown node alias: " ^ alias) )
  | _ ->
      failwith "Malformed proposal event spec"

let hydrate_metric_event sim ({data; id; time; _} : Sim_event.spec) =
  match data with
  | Some "print_bus_stats" ->
      { Sim_event.id= Option.value id ~default:(Simulator.next_event_id sim)
      ; time
      ; kind= Sim_event.Metric
      ; action= (fun () -> Simulator.print_bus_stats sim) }
  | _ ->
      failwith ("Unknown metric data: " ^ Option.value ~default:"" data)

let hydrate_control_event sim ({target; data; id; time; _} : Sim_event.spec) =
  let action =
    match (target, data) with
    | Some alias, Some "deactivate" ->
        fun () -> alias |> Simulator.make_node_inactive sim ~time
    | Some alias, Some "activate" | Some alias, Some "reactivate" ->
        fun () -> alias |> Simulator.make_node_idle sim ~time
    | _ ->
        failwith "Malformed control event spec"
  in
  { Sim_event.id= Option.value id ~default:(Simulator.next_event_id sim)
  ; time
  ; kind= Sim_event.Control
  ; action }

let hydrate_fallback sim (spec : Sim_event.spec) =
  let other = spec.kind in
  { Sim_event.id= Option.value spec.id ~default:(Simulator.next_event_id sim)
  ; time= spec.time
  ; kind= Sim_event.Custom other
  ; action=
      (* TODO: fix the sim control series of steps *)
      (fun () -> Stdio.printf "[Sim_event] Unhandled kind %s\n%!" other ) }

let hydrate_event sim (spec : Sim_event.spec) =
  match spec.kind with
  | "proposal" ->
      hydrate_proposal_event sim spec
  | "metric" ->
      hydrate_metric_event sim spec
  | "control" ->
      hydrate_control_event sim spec
  | _other ->
      hydrate_fallback sim spec

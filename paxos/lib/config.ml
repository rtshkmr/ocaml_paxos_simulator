open Simulator
open Sim_event


(* TODO: not sure what the value of this is anymore *)
type simulation_config =
  { preamble: string
  ; simulator: Simulator.spec
  ; nodes: Simulator.NodeImpl.spec list
  ; events: Sim_event.spec list }
[@@deriving yojson]

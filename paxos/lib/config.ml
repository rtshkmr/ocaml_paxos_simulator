open Simulator
open Sim_event

type simulation_config =
  { preamble: string
  ; simulator: Simulator.spec
  ; nodes: Simulator.NodeImpl.spec list
  ; events: Sim_event.spec list }
[@@deriving yojson]

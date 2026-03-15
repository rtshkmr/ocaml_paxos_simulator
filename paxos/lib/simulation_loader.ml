open Base
open Simulator
open Sim_event

type simulation_config = {
  scenario_name : string;
  preamble : string;
  simulator : Simulator.spec;
  nodes : Simulator.N.spec list;
  events : Sim_event.spec list;
}
[@@deriving yojson]

let load_simulation_config_from_file file_path =
  match file_path |> Yojson.Safe.from_file |> simulation_config_of_yojson with
  | Ok config -> config
  | Error e -> failwith ("Failed to parse simulation config: " ^ e)

(** %%%%%%%%%%%% DESIGN NOTE: Spec-Based Configuration as a Manual DSL %%%%%%%%%%%%

  This module illustrates a **declarative configuration system** implemented via
    a spec layer. The pattern is:

  1. User writes JSON scenario files (human-friendly declarations)
  2. Yojson parses JSON → OCaml spec types (load_simulation_config_from_file)
  3. Spec types undergo validation and hydration → runtime objects
  4. Runtime objects execute the simulation

  So, our approach uses a declarative spec layer to bridge JSON config files to runtime types, which is then hydrated.

  ## Why use specs?

  An alternative is to make runtime types directly serializable:
  ```ocaml
  type simulator_state = { ... } [@@deriving yojson]
  ```
    But this couples persistence to runtime representation.
    The spec layer we have now is a layer of indirection that decouples:
    - JSON format can evolve without breaking runtime
    - Validation logic is explicit and centralized
    - Hydration can fail gracefully

  ## The Manual DSL Approach

  The spec types (simulation_config, node_spec, sim_event_spec, etc.) form a
  **manually-maintained intermediate language**.
  Each spec has:
    - A type definition (e.g., type node_spec = {...})
    - Yojson derives for JSON marshalling
    - An of_spec converter to runtime types

  We make this deliberate trade-off for now. A proper DSL would use a parser
  to automatically generate specs and converters. We avoided that complexity
  because:
    1. Domain is small (~5-10 spec types)
    2. Manual specs are more explicit for pedagogical clarity
    3. Parsing infrastructure wasn't available in time scope

  However, should we evolve this project to a more comprehensive state, we should consider simplifying it

  ## Maintenance Cost

  The trade-off is **boilerplate**: adding a field to proposer_state requires
  updating proposer_state_spec, its yojson derive, and its of_spec converter.
  This is tedious but scalable for a project of this scope (v0).

  ## Future Improvements

  If the configuration domain grows, the next steps would be:

  ### Stage 1: Explicit Validation (most likely next step)
  ────────────────────────────────────────────────────

  Add a Validation module that checks invariants:

    - Cluster size must be > 0
    - Quorum size must be > cluster_size / 2
    - Event times must be monotonic
    - Node IDs must be unique

  This separates loading (JSON → spec) from validation (spec → errors),
  making both testable.

  ### Stage 2: Schema-Driven Codegen (If Domain Grows)
  ───────────────────────────────────────────────────

  Define a schema once:

    let node_schema = Schema.record "node" [
      ("node_id", Schema.int ~min:0);
      ("cluster_size", Schema.int ~min:1);
      ...
    ]

  Then generate:
    - Spec types
    - Yojson marshallers
    - Validators
    - CLI argument parsers

  Tools like Dhall, CUE, and Terraform use this approach. OCaml doesn't seem to have
  a standard tool, but it's feasible with library support.

  ### Stage 3: Parser-Based DSL (If Users Need Custom Syntax)
  ──────────────────────────────────────────────────────────

  Use Angstrom or Menhir to parse a Paxos-specific scenario language:

    scenario "caravan_bakeoff" {
      nodes: 5
      roles: [Proposer, Acceptor, Learner]

      event "at tick 10, partition node 0" {
        time: 10
        target: node[0]
        action: network_partition
      }
    }

  This requires learning OCaml's parser tooling, which improves readability
  at the cost of increased implementation complexity.

  ## What This Design Teaches

  The spec approach is common in real systems:

    - Kubernetes: YAML spec types + validation + controllers
    - Terraform: HCL configs + validators + resource generators
    - OCaml's dune: Stanzas (specs) + parsing + build logic
    - Elm: JSON specs for UI, compiled to HTML

  Most start exactly where we are: manual specs + converters. Evolution to
  automated codegen happens only when the domain justifies it.

  By documenting this explicitly, we acknowledge the design trade-off and
  leave a clear path for evolution.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*)

open Types

(** Global simulator configuration and per-node specs. *)

type node_spec =
  { node_id: int
  ; roles: string list (* e.g. ["Proposer"] *)
  ; topics: Types.topic list
  ; initial_quorum: int option
  ; initial_state: string option
  ; storage_config: string option }

type t =
  { max_ticks: int option
  ; deterministic_seed: int option
  ; default_quorum: int option
  ; initial_nodes: node_spec list
  ; log_jsonl: string option }

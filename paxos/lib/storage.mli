open Base
open Time

module type S = sig
  type snapshot_payload [@@deriving sexp, yojson]
  (** Abstract payload representing the entire node state snapshot. Kept generic
      and opaque to avoid coupling with node internals. Must be serializable
      with sexp and yojson. *)

  type t
  (** Abstract handle representing storage context or connection. Can be an
      in-memory map, file handle, DB connection, etc. *)

  type log_entry = { timestamp : Time.t; snapshot : snapshot_payload }
  [@@deriving sexp, yojson]
  (** A log entry for the distributed consensus record, containing:
      - [timestamp] when the entry was created or agreed upon
      - [snapshot] the node state snapshot at that point *)

  val create : alias:string -> unit -> t
  (** Create a new storage context/handle for persistence. *)

  val persist_snapshot :
    t -> Time.t -> snapshot_payload -> (t, Error.t) Result.t
  (** Persist a full node snapshot durably. Called on key state transitions.
      Returns [Ok ()] on success or [Error _] on failure. *)

  val load_snapshot : t -> (snapshot_payload option, Error.t) Result.t
  (** Load the most recent node snapshot, if any. Returns [Ok (Some _)] if
      found, [Ok None] if none, or [Error _] on failure. *)

  val load_consensus_log : t -> (log_entry list, Error.t) Result.t
  (** Load the full ordered consensus log. For large logs, consider
      streaming/batching. *)

  val compact_log : t -> (t, Error.t) Result.t
  (** Optional compaction or truncation to reduce log size for performance. *)
end

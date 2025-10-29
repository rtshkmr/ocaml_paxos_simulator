open Types


(**
  Message ADTs for Paxos.
  - Messages are parameterized by the value type `'v`.
  - Includes a small `Meta` submodule for common metadata that can be used by
    simulators, logs, or transports.
  - Exposes helper constructors and topic helpers.

  Intent:
  - Keep messages simple and serializable ([@@deriving sexp]).
  - Allow transports to attach or override Meta as needed.
*)
module Message : sig

  (** Metadata about a message that is useful for displaying.  *)
  module Meta : sig
    type t = {
      id : Uuidm.t;
      timestamp : Time.t;  (** Logical / real timestamp*)
      topic : Types.topic; (** Logical message topic*)
    }
    [@@deriving sexp, compare, equal]
  end

  (** Messages are parameterized by payload type ['v].
      The Paxos algo defines these 5 variants in its spec.*)
  type 'v t =
    | PermissionRequest of { meta : Meta.t; from : Types.node_id }
    | PermissionGranted of { meta : Meta.t; from : Types.node_id }
    | Suggestion of { meta : Meta.t; from : Types.node_id; value : 'v }
    | Accepted of { meta : Meta.t; from : Types.node_id; value : 'v }
    | Nack of { meta : Meta.t; from : Types.node_id }
  [@@deriving sexp, compare, equal]


  (** [topic_of] extracts the topic from any message. *)
  val topic_of : _ t -> Types.topic

  (** [sender_of] extracts the sender node ID. *)
  val sender_of : _ t -> Types.node_id

  (** Create a new message of a given topic*)
  val make: Types.topic -> ('v -> 'v t) -> 'v -> from:Types.node_id -> 'v t

end

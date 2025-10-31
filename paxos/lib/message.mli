open Base
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
    type t =
      { id: Uuidm.t
      ; timestamp: Time.t  (** Logical / real timestamp*)
      ; topic: Types.topic
            (** bus-level topic -- this is @ the simulation layer*) }
    [@@deriving sexp, compare, equal]
  end

  (** Messages are parameterized by payload type ['v].
      The Paxos algo defines these 5 variants in its spec.

      Notes:

      1. [Nack] variant has an optional [hint] which helps to inform about the highest promise seen.
  *)
  type 'v t =
    | PermissionRequest of
        { meta: Meta.t
        ; from: Types.node_id
        ; proposal: Types.proposal_id
        ; quorum: int option
        ; value: 'v }
    | PermissionGranted of
        { meta: Meta.t
        ; from: Types.node_id
        ; quorum: int option
        ; last_accepted: (Types.proposal_id * 'v) option }
    | Suggestion of
        { meta: Meta.t
        ; from: Types.node_id
        ; proposal: Types.proposal_id
        ; quorum: int option
        ; value: 'v }
    | Accepted of
        { meta: Meta.t
        ; from: Types.node_id
        ; proposal: Types.proposal_id
        ; quorum: int option
        ; value: 'v }
    | Nack of
        { meta: Meta.t
        ; from: Types.node_id
        ; quorum: int option
        ; hint: Types.proposal_id option }
  [@@deriving sexp, compare, equal]

  val topic_of : _ t -> Types.topic
  (** [topic_of] extracts the topic from any message. *)

  val sender_of : _ t -> Types.node_id
  (** [sender_of] extracts the sender node ID. *)

  val proposal_id_of : _ t -> Types.proposal_id option
  (** [proposal_id_of] extracts the proposal ID from the message, if it exists. *)

  val quorum_of : _ t -> int option
  (** [quorum_of] will extract the optional quorum from within the message  *)

  (* helpers to construct messages; ensure meta.topic matches provided topic *)
  val make_permission_request :
       topic:Types.topic
    -> from:Types.node_id
    -> proposal:Types.proposal_id
    -> value:'v
    -> quorum:int option
    -> 'v t

  val make_permission_granted :
       topic:Types.topic
    -> from:Types.node_id
    -> last_accepted:(Types.proposal_id * 'v) option
    -> quorum:int option
    -> 'v t

  val make_suggestion :
       topic:Types.topic
    -> from:Types.node_id
    -> proposal:Types.proposal_id
    -> value:'v
    -> quorum:int option
    -> 'v t

  val make_accepted :
       topic:Types.topic
    -> from:Types.node_id
    -> proposal:Types.proposal_id
    -> value:'v
    -> quorum:int option
    -> 'v t

  val make_nack :
       topic:Types.topic
    -> from:Types.node_id
    -> hint:Types.proposal_id option
    -> quorum:int option
    -> 'v t
end

open Base
open Types
open Time

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
      { id: int
      ; timestamp: Time.t  (** Logical / real timestamp*)
      ; topic: Types.topic
            (** bus-level topic -- this is @ the simulation layer*) }
    [@@deriving sexp, compare, equal]
  end

  type 'v permission_request_msg =
    {meta: Meta.t; from: Types.node_id; proposal: Types.proposal_id; value: 'v}
  [@@deriving sexp, compare, equal]

  val sexp_of_last_accepted :
    ('a -> Sexp.t) -> (Types.proposal_id * 'a) option -> Sexp.t

  type 'v permission_granted_msg =
    { meta: Meta.t
    ; from: Types.node_id
    ; proposal: Types.proposal_id
    ; last_accepted: (Types.proposal_id * 'v) option }
  [@@deriving sexp, compare, equal]

  type 'v suggestion_msg =
    {meta: Meta.t; from: Types.node_id; proposal: Types.proposal_id; value: 'v}
  [@@deriving sexp, compare, equal]

  type 'v accepted_msg =
    {meta: Meta.t; from: Types.node_id; proposal: Types.proposal_id; value: 'v}
  [@@deriving sexp, compare, equal]

  (**  [Nack] variant ([nack_msg] has an optional [hint] which helps to inform about the highest promise seen.
       this is intended for future use for nack optimisations @ the accepting stage.

       FIXME: the Message.Nack and State.nack don't play well together, they should have similar shapes.*)
  type 'v nack_msg =
    { meta: Meta.t
    ; proposal: Types.proposal_id
    ; from: Types.node_id
    ; hint: (Types.proposal_id * 'v) option }
  [@@deriving sexp, compare, equal]

  val sexp_of_nack_hint :
    ('a -> Sexp.t) -> (Types.proposal_id * 'a) option -> Sexp.t

  (** Messages are parameterized by payload type ['v].
      The Paxos algo defines these 5 variants in its spec for Coordination
  *)
  type 'v coordination_message =
    | PermissionRequest of 'v permission_request_msg
    | PermissionGranted of 'v permission_granted_msg
    | Suggestion of 'v suggestion_msg
    | Accepted of 'v accepted_msg
    | Nack of 'v nack_msg
  [@@deriving sexp, compare, equal]

  and 'v time_message =
    | Heartbeat of {meta: Meta.t; time: Time.t}
    | SyncTo of {meta: Meta.t; time: Time.t}
    | DiffOffset of {meta: Meta.t; diff: Time.t}
  [@@deriving sexp, compare, equal]

  and 'v simulation_control_message =
    | MakeNodeIdle of {meta: Meta.t; node_id: Types.node_id}
    | MakeNodeInactive of {meta: Meta.t; node_id: Types.node_id}
    | Pause of {meta: Meta.t}
    | Resume of {meta: Meta.t}
    | AdvanceTick of {meta: Meta.t}
    | Inject of {meta: Meta.t}
  [@@deriving sexp, compare, equal]

  and 'v t =
    | Coordination of 'v coordination_message
    | Control of 'v simulation_control_message
    | Time of 'v time_message
  [@@deriving sexp, compare, equal]

  val payload_serialiser_of : ('v -> Sexp.t) -> 'v t -> string

  val meta_of : _ t -> Meta.t

  val topic_of : _ t -> Types.topic
  (** [topic_of] extracts the topic from any message. *)

  val sender_of : _ t -> Types.node_id
  (** [sender_of] extracts the sender node ID. *)

  val proposal_id_of : _ t -> Types.proposal_id option
  (** [proposal_id_of] extracts the proposal ID from the message, if it exists. *)

  (* helpers to construct messages; ensure meta.topic matches provided topic *)
  val make_permission_request :
       msg_id:int
    -> topic:Types.topic
    -> time:Time.t
    -> from:Types.node_id
    -> proposal:Types.proposal_id
    -> value:'v
    -> 'v coordination_message

  val make_permission_granted :
       msg_id:int
    -> topic:Types.topic
    -> proposal:Types.proposal_id
    -> time:Time.t
    -> from:Types.node_id
    -> last_accepted:(Types.proposal_id * 'v) option
    -> 'v coordination_message

  val make_suggestion :
       msg_id:int
    -> topic:Types.topic
    -> time:Time.t
    -> from:Types.node_id
    -> proposal:Types.proposal_id
    -> value:'v
    -> 'v coordination_message

  val make_accepted :
       msg_id:int
    -> topic:Types.topic
    -> time:Time.t
    -> from:Types.node_id
    -> proposal:Types.proposal_id
    -> value:'v
    -> 'v coordination_message

  val make_nack :
       msg_id:int
    -> topic:Types.topic
    -> time:Time.t
    -> from:Types.node_id
    -> proposal:Types.proposal_id
    -> hint:(Types.proposal_id * 'v) option
    -> 'v coordination_message

  val make_sim_control_idle_node :
       msg_id:int
    -> time:Time.t
    -> node_id:Types.node_id
    -> 'v simulation_control_message

  val make_sim_control_inactive_node :
       msg_id:int
    -> time:Time.t
    -> node_id:Types.node_id
    -> 'v simulation_control_message

  val make_heartbeat_msg : msg_id:int -> time:Time.t -> 'a time_message
end

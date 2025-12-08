(*
Improvements for consideration:
==============================
1. TODO [DEFENSIVE] don't use the fail-with, use custom errors or something
*)
open Base
open Types
open Time

module Message = struct
  module Meta = struct
    type t = {id: int; timestamp: Time.t; topic: Types.topic}
    [@@deriving sexp, compare, equal]
  end

  type 'v permission_request_msg =
    { meta: Meta.t
    ; from: Types.node_id
    ; assertion: 'v Types.paxos_assertion_state }
  [@@deriving sexp, compare, equal]

  type 'v permission_granted_msg =
    { meta: Meta.t
    ; from: Types.node_id
    ; assertion: 'v Types.paxos_assertion_state
    ; last_accepted: 'v Types.paxos_promise }
  [@@deriving sexp, compare, equal]

  let sexp_of_last_accepted sexp_of_v = function
    | None ->
        Sexplib.Sexp.Atom "None"
    | Some (proposal_id, v) ->
        List [Types.sexp_of_proposal_id proposal_id; sexp_of_v v]

  type 'v suggestion_msg =
    { meta: Meta.t
    ; from: Types.node_id
    ; assertion: 'v Types.paxos_assertion_state }
  [@@deriving sexp, compare, equal]

  type 'v accepted_msg =
    { meta: Meta.t
    ; from: Types.node_id
    ; assertion: 'v Types.paxos_assertion_state }
  [@@deriving sexp, compare, equal]

  (**  [Nack] variant ([nack_msg] has an optional [hint] which helps to inform about the highest promise seen.
       this is intended for future use for nack optimisations @ the accepting stage.
       *)
  type 'v nack_msg =
    { meta: Meta.t
    ; from: Types.node_id
    ; rejected_assertion: 'v Types.paxos_assertion_state
    ; hint: 'v Types.paxos_promise }
  [@@deriving sexp, compare, equal]

  type 'v decided_msg =
    { meta: Meta.t
    ; from: Types.node_id
    ; decided_assertion: 'v Types.paxos_assertion_state }
  [@@deriving sexp, compare, equal]

  (** Messages are parameterized by payload type ['v].
      The Paxos algo defines these 5 variants in its spec for Coordination
  *)
  type 'v coordination_message =
    | PermissionRequest of 'v permission_request_msg
    | PermissionGranted of 'v permission_granted_msg
    | Suggestion of 'v suggestion_msg
    | Accepted of 'v accepted_msg
    | Nack of 'v nack_msg
    | Decided of 'v decided_msg
  [@@deriving sexp, compare, equal]

  and 'v time_message =
    | Heartbeat of {meta: Meta.t; time: Time.t}
    | SyncTo of {meta: Meta.t; time: Time.t}
    | DiffOffset of {meta: Meta.t; diff: Time.t}
  [@@deriving sexp, compare, equal]

  and 'v simulation_control_message =
    | MakeNodeIdle of {meta: Meta.t; node_id: Types.node_id}
    | ActivateNode of {meta: Meta.t; node_id: Types.node_id}
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

  let to_string sexp_of_v msg =
    msg |> sexp_of_t sexp_of_v |> Sexplib.Sexp.to_string_hum

  let make_meta id timestamp topic : Meta.t = {id; timestamp; topic}

  let coordination_meta = function
    | PermissionRequest {meta; _}
    | PermissionGranted {meta; _}
    | Suggestion {meta; _}
    | Accepted {meta; _}
    | Decided {meta; _}
    | Nack {meta; _} ->
        meta

  let control_meta = function
    | MakeNodeInactive {meta; _}
    | ActivateNode {meta; _}
    | MakeNodeIdle {meta; _}
    | Pause {meta}
    | Resume {meta}
    | AdvanceTick {meta}
    | Inject {meta} ->
        meta

  let time_meta = function
    | Heartbeat {meta; _} | SyncTo {meta; _} | DiffOffset {meta; _} ->
        meta

  let meta_of = function
    | Coordination msg ->
        msg |> coordination_meta
    | Control msg ->
        msg |> control_meta
    | Time msg ->
        msg |> time_meta

  let topic_of msg =
    let {topic; _} : Meta.t = msg |> meta_of in
    topic

  let coordination_sender = function
    | PermissionRequest {from; _}
    | PermissionGranted {from; _}
    | Suggestion {from; _}
    | Accepted {from; _}
    | Decided {from; _}
    | Nack {from; _} ->
        from

  let sender_of = function
    | Coordination msg ->
        msg |> coordination_sender
    | Control _ ->
        (* Control messages don't have a 'from' field; handle as needed *)
        failwith "sender_of: Control messages do not have a sender"
    | Time _ ->
        failwith "sender_of: Control messages do not have a sender"

  let coordination_proposal = function
    | PermissionRequest {assertion= {proposal; _}; _}
    | Suggestion {assertion= {proposal; _}; _}
    | Accepted {assertion= {proposal; _}; _} ->
        Some proposal
    | PermissionGranted {assertion= {proposal; _}; _} ->
        Some proposal
    | Decided {decided_assertion= {proposal; _}; _} ->
        Some proposal
    | Nack {rejected_assertion= {proposal; _}; _} ->
        Some proposal

  let proposal_id_of = function
    | Coordination msg ->
        msg |> coordination_proposal
    | _ ->
        None

  let make_heartbeat_msg ~msg_id ~time =
    {meta= make_meta msg_id time Types.Time; time} |> Heartbeat |> Time

  let make_sim_control_idle_node ~msg_id ~time ~node_id =
    {meta= make_meta msg_id time Types.Simulation_control; node_id}
    |> MakeNodeIdle |> Control

  let make_sim_control_inactive_node ~msg_id ~time ~node_id =
    {meta= make_meta msg_id time Types.Simulation_control; node_id}
    |> MakeNodeInactive |> Control

  let make_sim_control_activate_node ~msg_id ~time ~node_id =
    {meta= make_meta msg_id time Types.Simulation_control; node_id}
    |> ActivateNode |> Control

  let make_permission_request ~msg_id ~time ~from ~assertion =
    {meta= make_meta msg_id time Types.Coordination; from; assertion}
    |> PermissionRequest |> Coordination

  let make_permission_granted ~msg_id ~topic ~assertion ~time ~from
      ~last_accepted =
    {meta= make_meta msg_id time topic; from; assertion; last_accepted}
    |> PermissionGranted |> Coordination

  let make_suggestion ~msg_id ~time ~from ~assertion =
    {meta= make_meta msg_id time Types.Coordination; from; assertion}
    |> Suggestion |> Coordination

  let make_accepted ~msg_id ~topic ~time ~from ~assertion =
    {meta= make_meta msg_id time topic; from; assertion}
    |> Accepted |> Coordination

  let make_nack ~msg_id ~topic ~time ~from ~rejected_assertion ~hint =
    {meta= make_meta msg_id time topic; from; rejected_assertion; hint}
    |> Nack |> Coordination

  let make_decided ~msg_id ~time ~from ~decided_assertion =
    {meta= make_meta msg_id time Types.Coordination; from; decided_assertion}
    |> Decided |> Coordination
end

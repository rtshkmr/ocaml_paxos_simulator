open Base
open Types
open Time

module Message = struct
  module Meta = struct
    type t = {id: int; timestamp: Time.t; topic: Types.topic}
    [@@deriving sexp, compare, equal]
  end

  type 'v coordination_message =
    | PermissionRequest of
        { meta: Meta.t
        ; from: Types.node_id
        ; proposal: Types.proposal_id
        ; value: 'v }
    | PermissionGranted of
        { meta: Meta.t
        ; from: Types.node_id
        ; proposal: Types.proposal_id
        ; last_accepted: (Types.proposal_id * 'v) option }
    | Suggestion of
        { meta: Meta.t
        ; from: Types.node_id
        ; proposal: Types.proposal_id
        ; value: 'v }
    | Accepted of
        { meta: Meta.t
        ; from: Types.node_id
        ; proposal: Types.proposal_id
        ; value: 'v }
    | Nack of
        { meta: Meta.t
        ; proposal: Types.proposal_id
        ; from: Types.node_id
        ; hint: Types.proposal_id option }
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

  let make_meta id timestamp topic : Meta.t = {id; timestamp; topic}

  let meta_of = function
    | Coordination msg -> (
      match msg with
      | PermissionRequest {meta; _}
      | PermissionGranted {meta; _}
      | Suggestion {meta; _}
      | Accepted {meta; _}
      | Nack {meta; _} ->
          meta )
    | Control msg -> (
      match msg with
      | MakeNodeInactive {meta; _}
      | MakeNodeIdle {meta; _}
      | Pause {meta}
      | Resume {meta}
      | AdvanceTick {meta}
      | Inject {meta} ->
          meta )
    | Time msg -> (
      match msg with
      | Heartbeat {meta; _} | SyncTo {meta; _} | DiffOffset {meta; _} ->
          meta )

  let topic_of msg =
    let meta = meta_of msg in
    meta.topic

  let sender_of = function
    | Coordination msg -> (
      match msg with
      | PermissionRequest {from; _}
      | PermissionGranted {from; _}
      | Suggestion {from; _}
      | Accepted {from; _}
      | Nack {from; _} ->
          from )
    | Control _ ->
        (* Control messages don't have a 'from' field; handle as needed *)
        failwith "sender_of: Control messages do not have a sender"
    | Time _ ->
        failwith "sender_of: Control messages do not have a sender"

  let proposal_id_of = function
    | Coordination msg -> (
      match msg with
      | PermissionRequest {proposal; _}
      | Suggestion {proposal; _}
      | Accepted {proposal; _} ->
          Some proposal
      | PermissionGranted {last_accepted= Some (proposal, _); _} ->
          Some proposal
      | PermissionGranted {last_accepted= None; _} ->
          None
      | Nack {hint; _} ->
          hint )
    | Control _ ->
        None
    | Time _ ->
        None

  let make_heartbeat_msg ~msg_id ~time =
    let topic = Types.Time in
    let id = msg_id in
    let meta = make_meta id time topic in
    Heartbeat {meta; time}

  let make_sim_control_idle_node ~msg_id ~time ~node_id =
    let topic = Types.Simulation_control in
    let id = msg_id in
    let meta = make_meta id time topic in
    MakeNodeIdle {meta; node_id}

  let make_sim_control_inactive_node ~msg_id ~time ~node_id =
    let topic = Types.Simulation_control in
    let id = msg_id in
    let meta = make_meta id time topic in
    MakeNodeInactive {meta; node_id}

  let make_permission_request ~msg_id ~topic ~time ~from ~proposal ~value =
    let id = msg_id in
    let meta = make_meta id time topic in
    PermissionRequest {meta; from; proposal; value}

  let make_permission_granted ~msg_id ~topic ~proposal ~time ~from
      ~last_accepted =
    let id = msg_id in
    let meta = make_meta id time topic in
    PermissionGranted {meta; from; proposal; last_accepted}

  let make_suggestion ~msg_id ~topic ~time ~from ~proposal ~value =
    let id = msg_id in
    let meta = make_meta id time topic in
    Suggestion {meta; from; proposal; value}

  let make_accepted ~msg_id ~topic ~time ~from ~proposal ~value =
    let id = msg_id in
    let meta = make_meta id time topic in
    Accepted {meta; from; proposal; value}

  let make_nack ~msg_id ~topic ~time ~from ~proposal ~hint =
    let id = msg_id in
    let meta = make_meta id time topic in
    Nack {meta; from; proposal; hint}
end

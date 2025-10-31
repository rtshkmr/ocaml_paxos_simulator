open Base
open Types

module Message = struct
  (** Wrapper over Uuidm type so that the ppx derivation will work (the OG lib doesn't provide these functions.)*)
  module Uuid_sexp = struct
    type t = Uuidm.t

    let sexp_of_t (u : t) = Sexplib.Sexp.Atom (Uuidm.to_string u)

    let t_of_sexp = function
      | Sexplib.Sexp.Atom s -> (
        match Uuidm.of_string s with
        | Some u ->
            u
        | None ->
            failwith "Invalid UUID string for Uuid_sexp.t" )
      | _ ->
          failwith "Expected atom for Uuid_sexp.t"

    let compare (a : t) (b : t) =
      String.compare (Uuidm.to_string a) (Uuidm.to_string b)

    let equal (a : t) (b : t) =
      String.equal (Uuidm.to_string a) (Uuidm.to_string b)
  end

  module Meta = struct
    (* Manual conversion functions for Uuidm.t *)
    type t = {id: Uuid_sexp.t; timestamp: Time.t; topic: Types.topic}
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
    | Nack of {meta: Meta.t; from: Types.node_id; hint: Types.proposal_id option}
  [@@deriving sexp, compare, equal]

  and 'v simulation_control_message =
    | MakeNodeIdle of {meta: Meta.t; node_id: Types.node_id}
    | MakeNodeEcho of {meta: Meta.t; node_id: Types.node_id}
    | Pause of {meta: Meta.t}
    | Resume of {meta: Meta.t}
    | AdvanceTick of {meta: Meta.t}
    | Inject of {meta: Meta.t}
  [@@deriving sexp, compare, equal]

  and 'v t =
    | Coordination of 'v coordination_message
    | Control of 'v simulation_control_message
  [@@deriving sexp, compare, equal]

  let make_meta topic =
    {Meta.id= Uuidm.v4 (Bytes.create 16); timestamp= Unix.gettimeofday (); topic}

  let topic_of = function
    | Coordination msg -> (
      match msg with
      | PermissionRequest {meta; _}
      | PermissionGranted {meta; _}
      | Suggestion {meta; _}
      | Accepted {meta; _}
      | Nack {meta; _} ->
          meta.topic )
    | Control msg -> (
      match msg with
      | MakeNodeEcho {meta; _}
      | MakeNodeIdle {meta; _}
      | Pause {meta}
      | Resume {meta}
      | AdvanceTick {meta}
      | Inject {meta} ->
          meta.topic )

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

  let make_sim_control_idle_node node_id =
    MakeNodeIdle {meta= make_meta Types.Simulation_control; node_id}

  let make_sim_control_echo_node node_id =
    MakeNodeEcho {meta= make_meta Types.Simulation_control; node_id}

  let make_permission_request ~topic ~from ~proposal ~value =
    PermissionRequest {meta= make_meta topic; from; proposal; value}

  let make_permission_granted ~topic ~from ~last_accepted =
    PermissionGranted {meta= make_meta topic; from; last_accepted}

  let make_suggestion ~topic ~from ~proposal ~value =
    Suggestion {meta= make_meta topic; from; proposal; value}

  let make_accepted ~topic ~from ~proposal ~value =
    Accepted {meta= make_meta topic; from; proposal; value}

  let make_nack ~topic ~from ~hint = Nack {meta= make_meta topic; from; hint}
end

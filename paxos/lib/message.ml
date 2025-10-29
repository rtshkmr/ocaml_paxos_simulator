open Base
open Types

module Message = struct
  module Meta = struct
    type t = {
      id : Uuidm.t;
      timestamp : Time.t;
      topic : Types.topic;
    } [@@deriving sexp, compare, equal]
  end

  type 'v t =
    | PermissionRequest of { meta : Meta.t; from : Types.node_id; proposal : Types.proposal_id }
    | PermissionGranted of { meta : Meta.t; from : Types.node_id; last_accepted : (Types.proposal_id * 'v) option }
    | Suggestion of { meta : Meta.t; from : Types.node_id; proposal : Types.proposal_id; value : 'v }
    | Accepted of { meta : Meta.t; from : Types.node_id; proposal : Types.proposal_id; value : 'v }
    | Nack of { meta : Meta.t; from : Types.node_id; hint : Types.proposal_id option }
  [@@deriving sexp, compare, equal]

  let make_meta topic =
    { Meta.id = Uuidm.v4 (Bytes.create 16); timestamp = Unix.gettimeofday (); topic }

  let topic_of = function
    | PermissionRequest { meta; _ }
    | PermissionGranted { meta; _ }
    | Suggestion { meta; _ }
    | Accepted { meta; _ }
    | Nack { meta; _ } -> meta.topic

  let sender_of = function
    | PermissionRequest { from; _ }
    | PermissionGranted { from; _ }
    | Suggestion { from; _ }
    | Accepted { from; _ }
    | Nack { from; _ } -> from

  let make_permission_request ~topic ~from ~proposal =
    PermissionRequest { meta = make_meta topic; from; proposal }

  let make_permission_granted ~topic ~from ~last_accepted =
    PermissionGranted { meta = make_meta topic; from; last_accepted }

  let make_suggestion ~topic ~from ~proposal ~value =
    Suggestion { meta = make_meta topic; from; proposal; value }

  let make_accepted ~topic ~from ~proposal ~value =
    Accepted { meta = make_meta topic; from; proposal; value }

  let make_nack ~topic ~from ~hint =
    Nack { meta = make_meta topic; from; hint }
end

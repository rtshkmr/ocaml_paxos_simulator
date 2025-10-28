open Types

module Message = struct

  module Meta = struct
    type t = {
      id : Uuidm.t;
      timestamp : Time.t;
      topic : Types.topic;
    }
    [@@deriving sexp, compare, equal]
  end

  type 'v t =
    | PermissionRequest of { meta : Meta.t; from : Types.node_id }
    | PermissionGranted of { meta : Meta.t; from : Types.node_id }
    | Suggestion of { meta : Meta.t; from : Types.node_id; value : 'v }
    | Accepted of { meta : Meta.t; from : Types.node_id; value : 'v }
    | Nack of { meta : Meta.t; from : Types.node_id }
  [@@deriving sexp, compare, equal]


  let topic_of = function
    | PermissionRequest { meta; _ } (* beautiful or-patterns! *)
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
end

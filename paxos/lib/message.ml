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

  let make (topic : Types.topic) (construct : 'v -> 'v t) (value : 'v) ~(from : Types.node_id) : 'v t =
    let meta = {
      Meta.id = Uuidm.v4 (Bytes.create 16);
      timestamp = Unix.gettimeofday ();
      topic;
    } in
    construct value |> function
    | PermissionRequest _ | PermissionGranted _ | Nack _ as msg ->
      (* For now, these variants do not have a 'value', so construct cannot create them,
         so just build the message with meta and from -- I'm not sure if they should be carrying a value*)
      (match msg with
       | PermissionRequest _ -> PermissionRequest { meta; from }
       | PermissionGranted _ -> PermissionGranted { meta; from }
       | Nack _ -> Nack { meta; from }
       | _ -> assert false)
    | Suggestion _ as msg ->
      (match msg with
       | Suggestion { value; _ } -> Suggestion { meta; from; value }
       | _ -> assert false)
    | Accepted _ as msg ->
      (match msg with
       | Accepted { value; _ } -> Accepted { meta; from; value }
       | _ -> assert false)
end

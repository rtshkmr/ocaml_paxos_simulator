[@@@ocaml.warning "-27"] (** TODO: remove unused variable warnings*)

open Base
open Types

module Storage_mem (V : Value.S) = struct
  type key = Types.node_id [@@deriving sexp]
  type payload = V.t [@@deriving sexp]
  type value = {
    promised : Types.proposal_id option;
    accepted : (Types.proposal_id * payload) option;
  } [@@deriving sexp]

  type t = {
    tbl : (key, value) Hashtbl.Poly.t;
  } [@@deriving sexp]

  let create ?config () =
    { tbl = Hashtbl.Poly.create () }

  let persist t key (v : value) =
    Hashtbl.set t.tbl ~key ~data:v;
    Ok ()

  let load t key =
    Ok (Hashtbl.find t.tbl key)

  let snapshot _t = Ok ()

  let ensure_entry t key =
    match Hashtbl.find t.tbl key with
    | Some _ -> ()
    | None ->
      let default_value = {promised = None; accepted = None} in
      Hashtbl.set t.tbl ~key ~data:default_value

  let get_promised_id t key =
    match Hashtbl.find t.tbl key with
        | None -> None
        | Some value  -> match value.promised with
          | None -> None
          | Some proposal_id -> Some proposal_id

  let get_accepted_value t key =
    match Hashtbl.find t.tbl key with
        | None -> None
        | Some value  -> match value.accepted with
          | None -> None
          | Some (_proposal_id, accepted_val) -> Some accepted_val

  let update_promise t key proposal_id =
    ensure_entry t key;
    match Hashtbl.find t.tbl key with
    | Some v ->
      let new_v = {v with promised = Some proposal_id} in
      Hashtbl.set t.tbl ~key ~data:new_v
    | None -> assert false

  let update_accepted t key proposal_id accepted_value =
    ensure_entry t key;
      match Hashtbl.find t.tbl key with
  | Some v ->
      let new_v = {v with accepted = Some (proposal_id, accepted_value)} in
      Hashtbl.set t.tbl ~key ~data:new_v
  | None -> assert false

end

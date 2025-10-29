open Base
open Types
open Storage

module Storage_mem (V : Storage) = struct
  type key = Types.node_id
  type value = {
    promised : Types.proposal_id option;
    accepted : (Types.proposal_id * V.t) option;
  } [@@deriving sexp]

  type t = {
    tbl : (key, value) Hashtbl.Poly.t;
  }

  let create ?_config () =
    { tbl = Hashtbl.Poly.create () }

  let persist t ~key (v : value) =
    Hashtbl.set t.tbl ~key ~data:v;
    Ok ()

  let load t ~key =
    Ok (Hashtbl.find t.tbl key)

  let snapshot _t = Ok ()
end

open Base
open Time

module Make_mem_storage (NS : Node_state.S) :
  Storage.S with type snapshot_payload = NS.role_state = struct
  type snapshot_payload = NS.role_state [@@deriving sexp, yojson]

  type t = {
    snapshot : snapshot_payload option;
    log : (Time.t * snapshot_payload) list;
  }

  type log_entry = { timestamp : Time.t; snapshot : snapshot_payload }
  [@@deriving sexp, yojson]

  let create ~alias () =
    Stdio.printf "Storage crated for %s\n%!" alias;
    { snapshot = None; log = [] }

  let persist_snapshot { log; _ } time (p : snapshot_payload) =
    Ok { snapshot = Some p; log = (time, p) :: log }

  let load_snapshot ({ snapshot; _ } : t) = Ok snapshot

  let load_consensus_log { log; _ } =
    Ok
      (log |> List.map ~f:(fun (timestamp, snapshot) -> { timestamp; snapshot }))

  let compact_log ({ log; _ } as t) =
    match List.rev log with
    | [] -> Ok t
    | (latest_t, latest_snapshot) :: _ ->
        Ok { t with log = [ (latest_t, latest_snapshot) ] }
end

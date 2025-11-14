open Base
open Time

module Make_mem_storage (NS : Node_state.S) :
  Storage.S with type snapshot_payload = NS.role_state = struct
  type snapshot_payload = NS.role_state [@@deriving sexp, yojson]

  type t =
    {snapshot: snapshot_payload option; log: (Time.t * snapshot_payload) list}

  type log_entry = {timestamp: Time.t; snapshot: snapshot_payload}
  [@@deriving sexp, yojson]

  let create ~alias () =
    Stdio.printf "Storage crated for %s\n%!" alias ;
    {snapshot= None; log= []}

  let persist_snapshot t time (p : snapshot_payload) =
    let updated_log = (time, p) :: t.log in
    let updated_snapshot = Some p in
    Ok {snapshot= updated_snapshot; log= updated_log}

  let load_snapshot (t : t) : (snapshot_payload option, Error.t) Result.t =
    let snapshot = t.snapshot in
    Ok snapshot

  let load_consensus_log t =
    let log_entries =
      List.map t.log ~f:(fun (timestamp, snapshot) -> {timestamp; snapshot})
    in
    Ok log_entries

  let compact_log t =
    match List.rev t.log with
    | [] ->
        Ok t
    | (latest_t, latest_snapshot) :: _ ->
        Ok {t with log= [(latest_t, latest_snapshot)]}
end

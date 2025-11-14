open Base
open Time

module Make_file_storage (NS : Node_state.S) :
  Storage.S with type snapshot_payload = NS.role_state = struct
  type snapshot_payload = NS.role_state [@@deriving sexp, yojson]

  let storage_dir = "/tmp/paxos"

  type t =
    { snapshot: snapshot_payload option
    ; log: (Time.t * snapshot_payload) list
    ; filepath: string }

  type log_entry = {timestamp: Time.t; snapshot: snapshot_payload}
  [@@deriving sexp, yojson]

  (* (\* Persisted format for the entire storage *\) *)
  type persisted_data =
    {snapshot: snapshot_payload option; log: (Time.t * snapshot_payload) list}
  [@@deriving yojson]

  let ensure_directory_exists dir =
    try
      if not (Stdlib.Sys.file_exists dir) then Stdlib.Sys.mkdir dir 0o755 ;
      Ok ()
    with exn -> Error (Error.of_exn exn)

  let filepath_for_alias alias =
    alias ^ "_node.json" |> Stdlib.Filename.concat storage_dir

  let create ~alias () =
    match ensure_directory_exists storage_dir with
    | Error e ->
        (* Log error but return empty storage *)
        Stdio.eprintf "Warning: Could not create storage directory: %s\n%!"
          (Error.to_string_hum e) ;
        {snapshot= None; log= []; filepath= alias |> filepath_for_alias}
    | Ok () ->
        {snapshot= None; log= []; filepath= filepath_for_alias alias}

  let write_to_file filepath data =
    try
      let json = persisted_data_to_yojson data in
      let json_string = Yojson.Safe.pretty_to_string json in
      Stdio.Out_channel.write_all filepath ~data:json_string ;
      Ok ()
    with exn -> Error (Error.of_exn exn)

  let read_from_file filepath =
    try
      if not (Stdlib.Sys.file_exists filepath) then Ok None
      else
        let json_string = Stdio.In_channel.read_all filepath in
        let json = Yojson.Safe.from_string json_string in
        match persisted_data_of_yojson json with
        | Ok data ->
            Ok (Some data)
        | Error msg ->
            Error (Error.of_string ("JSON deserialization error: " ^ msg))
    with exn -> Error (Error.of_exn exn)

  let persist_snapshot (t : t) time (p : snapshot_payload) =
    let updated_log = (time, p) :: t.log in
    let updated_snapshot = Some p in
    let updated_t = {t with snapshot= updated_snapshot; log= updated_log} in
    let data = {snapshot= updated_snapshot; log= updated_log} in
    match write_to_file t.filepath data with
    | Ok () ->
        Ok updated_t
    | Error e ->
        Error e

  let load_snapshot (t : t) : (snapshot_payload option, Error.t) Result.t =
    match read_from_file t.filepath with
    | Ok None ->
        Ok None
    | Ok (Some data) ->
        Ok data.snapshot
    | Error e ->
        Error e

  let load_consensus_log t =
    match read_from_file t.filepath with
    | Ok None ->
        Ok []
    | Ok (Some data) ->
        let log_entries =
          List.map data.log ~f:(fun (timestamp, snapshot) ->
              {timestamp; snapshot} )
        in
        Ok log_entries
    | Error e ->
        Error e

  let compact_log (t : t) =
    match List.rev t.log with
    | [] ->
        Ok t
    | (latest_t, latest_snapshot) :: _ -> (
        let compacted_t = {t with log= [(latest_t, latest_snapshot)]} in
        let data = {snapshot= compacted_t.snapshot; log= compacted_t.log} in
        match write_to_file t.filepath data with
        | Ok () ->
            Ok compacted_t
        | Error e ->
            Error e )
end

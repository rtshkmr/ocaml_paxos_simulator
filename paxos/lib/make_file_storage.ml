open Base
open Time

module Make_file_storage (NS : Node_state.S) :
  Storage.S with type snapshot_payload = NS.role_state = struct
  type snapshot_payload = NS.role_state [@@deriving sexp, yojson]

  let storage_dir = "/tmp/paxos"

  type t = {
    snapshot : snapshot_payload option;
    log : (Time.t * snapshot_payload) list;
    filepath : string;
  }

  type log_entry = { timestamp : Time.t; snapshot : snapshot_payload }
  [@@deriving sexp, yojson]

  (* (\* Persisted format for the entire storage *\) *)
  type persisted_data = {
    snapshot : snapshot_payload option;
    log : (Time.t * snapshot_payload) list;
  }
  [@@deriving yojson]

  (** Octal representation of permission:
      - The owner has read, write, and execute permissions.
      - The group and others have read and execute permissions, but not write.*)
  let owner_rwx_perms = 0o755

  let ensure_directory_exists dir =
    try
      if not (Stdlib.Sys.file_exists dir) then
        Stdlib.Sys.mkdir dir owner_rwx_perms;
      Ok ()
    with exn -> Error (Error.of_exn exn)

  let filepath_for_alias alias =
    alias ^ "_node.json" |> Stdlib.Filename.concat storage_dir

  let create ~alias () =
    match ensure_directory_exists storage_dir with
    | Error e ->
        (* Logs error but returns empty storage *)
        Stdio.eprintf "Warning: Could not create storage directory: %s\n%!"
          (Error.to_string_hum e);
        { snapshot = None; log = []; filepath = alias |> filepath_for_alias }
    | Ok () ->
        { snapshot = None; log = []; filepath = alias |> filepath_for_alias }

  (** Atomic file writing trick for reference.

      NOTE: this is suposedly make it non-portable code, but it's alright we can
      add an assumption that it's for Unix. refs:
      - https://lists.racket-lang.org/dev/archive/2011-January/005420.html
      - https://stackoverflow.com/questions/30385225/is-there-an-os-independent-way-to-atomically-overwrite-a-file/30549434#30549434
  *)
  let atomic_write_exn filepath contents =
    let tmp = filepath ^ ".tmp" in
    Stdio.Out_channel.write_all tmp ~data:contents;
    Unix.rename tmp filepath

  let write_to_file filepath data =
    try
      data |> persisted_data_to_yojson |> Yojson.Safe.pretty_to_string
      |> atomic_write_exn filepath;
      Ok ()
    with exn -> exn |> Error.of_exn |> Error

  let read_from_file filepath =
    try
      if not (Stdlib.Sys.file_exists filepath) then Ok None
      else
        let extraction_outcome =
          filepath |> Stdio.In_channel.read_all |> Yojson.Safe.from_string
          |> persisted_data_of_yojson
        in
        match extraction_outcome with
        | Ok data -> Ok (Some data)
        | Error msg ->
            Error (Error.of_string ("JSON deserialization error: " ^ msg))
    with exn -> exn |> Error.of_exn |> Error

  let persist_snapshot ({ filepath; log; _ } as t : t) time
      (p : snapshot_payload) =
    let updated_log = (time, p) :: log in
    let updated_snapshot = Some p in
    let updated_t = { t with snapshot = updated_snapshot; log = updated_log } in
    match
      { snapshot = updated_snapshot; log = updated_log }
      |> write_to_file filepath
    with
    | Ok () -> Ok updated_t
    | Error e -> Error e

  let load_snapshot ({ filepath; _ } : t) =
    match read_from_file filepath with
    | Ok None -> Ok None
    | Ok (Some data) -> Ok data.snapshot
    | Error e -> Error e

  let load_consensus_log { filepath; _ } =
    match read_from_file filepath with
    | Ok None -> Ok []
    | Ok (Some { log; _ }) ->
        Ok
          (log
          |> List.map ~f:(fun (timestamp, snapshot) -> { timestamp; snapshot })
          )
    | Error e -> Error e

  let compact_log ({ log; filepath; _ } as t : t) =
    match List.rev log with
    | [] -> Ok t
    | (latest_t, latest_snapshot) :: _ -> (
        let ({ snapshot; log; _ } as compacted_t) : t =
          { t with log = [ (latest_t, latest_snapshot) ] }
        in
        match { snapshot; log } |> write_to_file filepath with
        | Ok () -> Ok compacted_t
        | Error e -> Error e)
end

(*
TODO POSSBILE IMPROVEMENTS:
=====================
1. Defensive:
    a) File IO:
      - file IO is classically hairy, for nowmy zero-deps approach is the rason why i'm not offloading it to other packages in the ecosystem. I would never claim to handle this myself with complete guarantee of correctness :')

2. Better Error handling:
    a) why are we catching and throwing the same error? we should be adding some context to it e.g. by wrapping into a custom error. For now, I'll just leave it as is.
 *)

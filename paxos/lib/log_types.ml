open Base

module Log_level = struct
  type t = Debug | Info | Warn | Error [@@deriving sexp, equal, enumerate]

  let to_int = function Debug -> 0 | Info -> 1 | Warn -> 2 | Error -> 3

  let compare a b = Int.compare (a |> to_int) (b |> to_int)

  let should_log (curr : t) (min_required : t) : bool =
    (* let open Base in *)
    let compared : int = compare curr min_required in
    compared >= 0

  let of_int = function
    | 0 ->
        Debug
    | 1 ->
        Info
    | 2 ->
        Warn
    | 3 ->
        Error
    | _ ->
        Info

  let to_string = function
    | Debug ->
        "DEBUG"
    | Info ->
        "INFO"
    | Warn ->
        "WARN"
    | Error ->
        "ERROR"

  let colorizer_of t =
    let open Ansi.Formatter in
    match t with
    | Debug ->
        magenta
    | Info ->
        cyan
    | Warn ->
        fun s -> s |> bold |> bg_pastel_rose |> fg_muted_plum
    | Error ->
        bright_red

  let arg_type =
    Command.Arg_type.of_alist_exn
      [("debug", Debug); ("info", Info); ("warn", Warn); ("error", Error)]

  let flag = "-max-log-level"

  let doc = "LEVEL (debug|info|warn|error). Default=info"
end

(** Keep events as structured data. Call sites will pass domain-serialized
   strings (for topics, payload) to avoid logger depending on domain modules. *)
module Log_event = struct
  type t =
    | Publish_broadcast of {bus_id: int; topic_s: string; payload: string}
    | Publish_unicast of
        { bus_id: int
        ; sender_id_s: string option
        ; sender_alias: string option
        ; target_node: int
        ; topic_s: string
        ; payload: string }
    | Subscribe of
        {bus_id: int; topic_s: string; alias: string; node_id: int; sub_id: int}
    | Unsubscribe of
        {bus_id: int; topic_s: string; alias: string; node_id: int; sub_id: int}
    | Enqueue of {bus_id: int; topic_s: string; queue_size: int; alias: string}
    | Drain_start of {bus_id: int; batch_size: int}
    | Drain_end of {bus_id: int; batch_size: int}
    | Tick of {tick: string; msg: string option}
    | Subroutine_flow of
        { routine: string
        ; msg: string option
        ; node_id: int option
        ; alias: string option }
    | Decision of {alias: string option; node_id: int option; msg: string}
    | Reaction of {alias: string option; node_id: int option; msg: string}
    | Node_state_change of
        { alias: string option
        ; node_id: int
        ; old_state: string
        ; new_state: string }
    | Stats of {bus_id: int; dump: string}
    | Display_scenario_preamble of {scenario_name: string; preamble: string}
    | Other of string
  [@@deriving sexp_of]
end

module Entry = struct
  type t =
    { level: Log_level.t
    ; time: Time_float_unix.t
    ; node_id: int option
    ; alias: string option
    ; module_name: string option
    ; event: Log_event.t }
  [@@deriving sexp_of]

  let make ?node_id ?alias ?module_name ~level ~event () =
    {level; time= Time_float_unix.now (); node_id; alias; module_name; event}
end

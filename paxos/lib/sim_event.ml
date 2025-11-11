open Base
open Time

module Sim_event = struct
  type kind =
    | MessageDispatch
    | NodeAction
    | Metric
    | Control
    | Custom of string
  [@@deriving sexp, compare, equal]

  type t = {id: int; time: Time.t; kind: kind; action: unit -> unit}

  type spec =
    { id: int option
    ; time: int
    ; kind: string
    ; target: string option
    ; data: string option }
  [@@deriving sexp, yojson]

  (** Transformation from spec (plain description) into a runtime event *)
  let of_spec spec =
    let kind =
      match spec.kind with
      | "msg" | "message" ->
          MessageDispatch
      | "metric" ->
          Metric
      | "control" ->
          Control
      | "node" | "action" ->
          NodeAction
      | other ->
          Custom other
    in
    { id= Option.value spec.id ~default:(-1)
    ; time= spec.time
    ; kind
    ; action=
        (fun () ->
          Stdio.printf
            "[Sim_event stub] executing event kind=%s time=%d target=%s\n%!"
            spec.kind spec.time
            (Option.value spec.target ~default:"none") ) }

  let describe ({id; kind; time; _} : t) =
    Printf.sprintf "[Event #%d t=%d kind=%s]" id time
      (Sexp.to_string_hum (sexp_of_kind kind))
end

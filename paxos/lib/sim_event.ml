open Base
open Time

module Sim_event = struct
  type kind =
    | MessageDispatch
    | NodeAction
    | Metric
    | Control
    | Narration
    | Custom of string
  [@@deriving sexp, compare, equal]

  type t = {
    id : int;
    time : Time.t;
    args : string list option;
    kind : kind;
    action : unit -> unit;
  }

  type spec = {
    id : int option; [@default None] [@yojson_drop_default]
    time : int;
    kind : string;
    target : string option; [@default None] [@yojson_drop_default]
    args : string list option; [@default None] [@yojson_drop_default]
    data : string option; [@default None] [@yojson_drop_default]
  }
  [@@deriving sexp, yojson]

  let event_spec_to_event_kind = function
    | "msg" | "message" -> MessageDispatch
    | "metric" -> Metric
    | "control" -> Control
    | "node" | "action" -> NodeAction
    | other -> Custom other

  (** Transformation from spec (plain description) into a runtime event *)
  let of_spec ({ id; kind; time; target; args; _ } : spec) =
    {
      id = Option.value id ~default:(-1);
      time;
      kind = kind |> event_spec_to_event_kind;
      action =
        (fun () ->
          Stdio.printf
            "[Sim_event stub] executing event kind=%s time=%d target=%s\n%!"
            kind time
            (Option.value target ~default:"none"));
      args;
    }

  let describe ({ id; kind; time; _ } : t) =
    Printf.sprintf "[Event #%d t=%d kind=%s]" id time
      (Sexp.to_string_hum (sexp_of_kind kind))
end

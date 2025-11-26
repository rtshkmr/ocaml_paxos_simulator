(** Abstract interface for a UI backend that renders Log entries. *)
module type S = sig
  val format_entry : ?ignore_header:bool -> Log_types.Entry.t -> string
end

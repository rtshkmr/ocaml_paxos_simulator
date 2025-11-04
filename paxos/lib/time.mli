open Base

module Time : sig
  (* type t = Time.System.t [@@deriving sexp, compare, equal] *)
  type t = int [@@deriving sexp, compare, equal]

  val zero : t

  val increment : t -> t

  (* Clock manages logical time *)
  type clock

  val create_clock : unit -> clock

  val now : clock -> t

  val tick : clock -> unit

  val format_tick_msg : clock -> ?msg:string -> unit -> string
end

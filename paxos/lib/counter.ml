module Counter : sig
  type t

  val create : int -> t
  val next : t -> int
end = struct
  type t = int ref

  let create start = ref start

  let next counter =
    let v = !counter in
    counter := v + 1;
    v
end
(*
TODO [Improvements]
Improvements for Consideration:
==============================
1. not sure if it's important to guard against overflows, though I don't expect to encounter them for this project
*)

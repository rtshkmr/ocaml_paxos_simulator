module Counter : sig
  type t

  val create : int -> t

  val next : t -> int
end = struct
  type t = int ref

  let create start = ref start

  let next counter =
    let v = !counter in
    counter := v + 1 ;
    v
end

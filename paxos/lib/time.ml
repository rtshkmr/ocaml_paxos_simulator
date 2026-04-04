open Base

module Time = struct
  type t = int [@@deriving sexp, compare, equal, yojson]

  let zero = 0
  let increment t = t + 1

  type clock = { mutable current : t }

  let create_clock () = { current = zero }
  let now c = c.current
  let tick c = c.current <- c |> now |> increment
end

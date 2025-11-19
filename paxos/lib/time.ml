open Base

module Time = struct
  type t = int [@@deriving sexp, compare, equal, yojson]

  let zero = 0

  let increment t = t + 1

  type clock = {mutable current: t}

  let create_clock () = {current= zero}

  let now c = c.current

  (* let timestamp_of_now c = Int.to_string_hum (now c) *)

  let tick c = c.current <- increment c.current
  (* Logger.tick c.logger ~timestamp:(timestamp_of_now c) *)
  (*   ~msg:"...time is now stopped @ this tick for us to inspect" () *)
end

open Base
open Log

module Time = struct
  type t = int [@@deriving sexp, compare, equal, yojson]

  let zero = 0

  let increment t = t + 1

  type clock = {mutable current: t; logger: string Logger.t}

  let create_clock () = {current= zero; logger= Logger.create ()}

  let now c = c.current

  let timestamp_of_now c = Int.to_string_hum (now c)

  let tick c =
    c.current <- increment c.current ;
    Logger.log_tick c.logger (timestamp_of_now c)
      ~msg:"...time is now stopped @ this tick for us to inspect" ()
end

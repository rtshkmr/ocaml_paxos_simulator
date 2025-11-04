open Base
open Color

module Time = struct
  type t = int [@@deriving sexp, compare, equal]

  let zero = 0

  let increment t = t + 1

  type clock = {mutable current: t}

  let create_clock () = {current= zero}

  let now c = c.current

  let tick c = c.current <- increment c.current

  let format_tick_msg c ?(msg = "") () =
    let curr_tick = now c in
    let curr_tick_str = Int.to_string curr_tick in
    let tick_tag =
      Color.underline
        (Color.blink
           (Color.bold
              (Color.bg_white
                 (Color.bright_blue (Printf.sprintf "[Tick %s]" curr_tick_str)) ) ) )
    in
    tick_tag ^ " " ^ msg
end

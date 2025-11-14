(** A custom coloriser, we shall adapt to ocolor next time and this will wrap around ocolor*)
module Formatter = struct
  let reset = "\027[0m"

  let black s = "\027[30m" ^ s ^ reset

  let red s = "\027[31m" ^ s ^ reset

  let green s = "\027[32m" ^ s ^ reset

  let yellow s = "\027[33m" ^ s ^ reset

  let blue s = "\027[34m" ^ s ^ reset

  let magenta s = "\027[35m" ^ s ^ reset

  let cyan s = "\027[36m" ^ s ^ reset

  let white s = "\027[37m" ^ s ^ reset

  let bright_black s = "\027[90m" ^ s ^ reset

  let bright_red s = "\027[91m" ^ s ^ reset

  let bright_green s = "\027[92m" ^ s ^ reset

  let bright_yellow s = "\027[93m" ^ s ^ reset

  let bright_blue s = "\027[94m" ^ s ^ reset

  let bright_magenta s = "\027[95m" ^ s ^ reset

  let bright_cyan s = "\027[96m" ^ s ^ reset

  let bright_white s = "\027[97m" ^ s ^ reset

  let bold s = "\027[1m" ^ s ^ reset

  let dim s = "\027[2m" ^ s ^ reset

  let italic s = "\027[3m" ^ s ^ reset

  let underline s = "\027[4m" ^ s ^ reset

  let blink_ s = "\027[5m" ^ s ^ reset

  let reverse s = "\027[7m" ^ s ^ reset

  let hidden s = "\027[8m" ^ s ^ reset

  (* Add background colors too if desired *)
  let bg_red s = "\027[41m" ^ s ^ reset

  let bg_green s = "\027[42m" ^ s ^ reset

  let bg_yellow s = "\027[43m" ^ s ^ reset

  let bg_blue s = "\027[44m" ^ s ^ reset

  let bg_magenta s = "\027[45m" ^ s ^ reset

  let bg_cyan s = "\027[46m" ^ s ^ reset

  let bg_white s = "\027[47m" ^ s ^ reset

  (* Add more colors or styles as needed *)

  (* Helper to wrap text with given color function *)
  let colorize color_func text = color_func text

  (* Example with format *)
  let redf fmt = Printf.sprintf ("\027[31m" ^^ fmt ^^ "\027[0m")
end

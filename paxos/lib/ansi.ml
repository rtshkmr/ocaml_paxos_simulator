open Base

(** A custom coloriser, full ANSI support with wrappers; see gist for RGB and 256-color support. *)
module Formatter = struct
  let pad_string ?(char = ' ') ?(do_left = true) ?(do_right = true) pad s =
    let pad_str = String.init pad ~f:(Fn.const char) in
    let left_pad = if do_left then pad_str else "" in
    let right_pad = if do_right then pad_str else "" in
    left_pad ^ s ^ right_pad

  let get_terminal_width () =
    let open Stdio in
    let ic = Unix.open_process_in "tput cols" in
    try
      let line = Option.value (In_channel.input_line ic) ~default:"80" in
      ignore (Unix.close_process_in ic) ;
      Int.of_string line
    with _ -> 80

  (** Strips ansi sequences and gives string length*)
  let get_visible_length s =
    let ansi_re = Re.Perl.re "\\x1b\\[[0-9;]*m" |> Re.compile in
    let stripped = Re.replace ansi_re ~f:(fun _ -> "") s in
    let len = String.length stripped in
    let rec next_char i acc =
      if i >= len then acc
      else
        let c = Char.to_int stripped.[i] in
        if c < 0x80 then next_char (i + 1) (acc + 1)
        else if c land 0xE0 = 0xC0 then next_char (i + 2) (acc + 1)
        else if c land 0xF0 = 0xE0 then next_char (i + 3) (acc + 1)
        else if c land 0xF8 = 0xF0 then next_char (i + 4) (acc + 1)
        else next_char (i + 1) (acc + 1)
    in
    next_char 0 0

  let center_text s =
    let term_width = get_terminal_width () in
    let str_width = s |> get_visible_length in
    if str_width >= term_width then s
    else
      let total_pad = term_width - str_width in
      let half_pad = total_pad / 2 in
      pad_string ~char:' ' half_pad s

  let center_text_multiline s =
    let lines = String.split_lines s in
    let centered_lines = List.map lines ~f:center_text in
    String.concat ~sep:"\n" centered_lines

  let ansi_wrap code s = Printf.sprintf "\027[%sm%s\027[0m" code s

  (* --- Regular Colors (Foreground) --- *)
  let black s = ansi_wrap "30" s

  let red s = ansi_wrap "31" s

  let green s = ansi_wrap "32" s

  let yellow s = ansi_wrap "33" s

  let blue s = ansi_wrap "34" s

  let magenta s = ansi_wrap "35" s

  let cyan s = ansi_wrap "36" s

  let white s = ansi_wrap "37" s

  (* --- Bold Colors --- *)
  let bold_black s = ansi_wrap "1;30" s

  let bold_red s = ansi_wrap "1;31" s

  let bold_green s = ansi_wrap "1;32" s

  let bold_yellow s = ansi_wrap "1;33" s

  let bold_blue s = ansi_wrap "1;34" s

  let bold_magenta s = ansi_wrap "1;35" s

  let bold_cyan s = ansi_wrap "1;36" s

  let bold_white s = ansi_wrap "1;37" s

  (* --- Underline Colors --- *)
  let underline_black s = ansi_wrap "4;30" s

  let underline_red s = ansi_wrap "4;31" s

  let underline_green s = ansi_wrap "4;32" s

  let underline_yellow s = ansi_wrap "4;33" s

  let underline_blue s = ansi_wrap "4;34" s

  let underline_magenta s = ansi_wrap "4;35" s

  let underline_cyan s = ansi_wrap "4;36" s

  let underline_white s = ansi_wrap "4;37" s

  (* --- Background Colors --- *)
  let bg_black s = ansi_wrap "40" s

  let bg_red s = ansi_wrap "41" s

  let bg_green s = ansi_wrap "42" s

  let bg_yellow s = ansi_wrap "43" s

  let bg_blue s = ansi_wrap "44" s

  let bg_magenta s = ansi_wrap "45" s

  let bg_cyan s = ansi_wrap "46" s

  let bg_white s = ansi_wrap "47" s

  (* --- High Intensity Foreground --- *)
  let bright_black s = ansi_wrap "90" s

  let bright_red s = ansi_wrap "91" s

  let bright_green s = ansi_wrap "92" s

  let bright_yellow s = ansi_wrap "93" s

  let bright_blue s = ansi_wrap "94" s

  let bright_magenta s = ansi_wrap "95" s

  let bright_cyan s = ansi_wrap "96" s

  let bright_white s = ansi_wrap "97" s

  (* --- Bold High Intensity Foreground --- *)
  let bold_bright_black s = ansi_wrap "1;90" s

  let bold_bright_red s = ansi_wrap "1;91" s

  let bold_bright_green s = ansi_wrap "1;92" s

  let bold_bright_yellow s = ansi_wrap "1;93" s

  let bold_bright_blue s = ansi_wrap "1;94" s

  let bold_bright_magenta s = ansi_wrap "1;95" s

  let bold_bright_cyan s = ansi_wrap "1;96" s

  let bold_bright_white s = ansi_wrap "1;97" s

  (* --- High Intensity Backgrounds --- *)
  let bg_bright_black s = ansi_wrap "100" s

  let bg_bright_red s = ansi_wrap "101" s

  let bg_bright_green s = ansi_wrap "102" s

  let bg_bright_yellow s = ansi_wrap "103" s

  let bg_bright_blue s = ansi_wrap "104" s

  let bg_bright_magenta s = ansi_wrap "105" s

  let bg_bright_cyan s = ansi_wrap "106" s

  let bg_bright_white s = ansi_wrap "107" s

  let bold s = ansi_wrap "1" s

  let dim s = ansi_wrap "2" s

  let italic s = ansi_wrap "3" s

  let underline s = ansi_wrap "4" s

  let blink s = ansi_wrap "5" s

  let reverse s = ansi_wrap "7" s

  let hidden s = ansi_wrap "8" s

  let strikethrough s = ansi_wrap "9" s

  let fg_256 n s = ansi_wrap (Printf.sprintf "38;5;%d" n) s

  let bg_256 n s = ansi_wrap (Printf.sprintf "48;5;%d" n) s

  let reset s = ansi_wrap "0" s

  let fg_rgb r g b s = ansi_wrap (Printf.sprintf "38;2;%d;%d;%d" r g b) s

  let bg_rgb r g b s = ansi_wrap (Printf.sprintf "48;2;%d;%d;%d" r g b) s

  (* ---------- custom colour definitions --------- *)

  let light_pastel_yellow_bg = bg_rgb 240 230 140

  let dark_goldenrod_brown_fg = fg_rgb 90 70 20

  let muted_sage_green_bg = bg_rgb 200 210 180

  let dark_olive_green_fg = fg_rgb 40 50 30

  (* base colour pastel version*)
  let bg_pastel_blue = bg_rgb 200 225 245

  let fg_pastel_blue = fg_rgb 40 60 90

  (* base colour pastel version*)
  let bg_pastel_yellow = bg_rgb 252 244 207

  let fg_pastel_yellow = fg_rgb 120 90 40

  (* base colour pastel version*)
  let bg_pastel_green = bg_rgb 210 235 220

  let fg_pastel_green = fg_rgb 50 70 50

  (* base colour pastel version*)
  let bg_pastel_red = bg_rgb 250 215 210

  let fg_pastel_red = fg_rgb 130 60 52

  (* base colour pastel version*)
  let bg_pastel_magenta = bg_rgb 245 215 225

  let fg_pastel_magenta = fg_rgb 90 60 80

  (* yellow pastel pair -- bright *)

  (* let bg_pastel_yellow = bg_rgb 252 244 207 *)

  let fg_warm_brown = fg_rgb 120 90 40

  (* green pastel pair -- bright *)
  let bg_pastel_mint = bg_rgb 210 235 220

  let fg_deep_olive = fg_rgb 50 70 50

  (* blue pastel pair -- bright *)
  let bg_pastel_powder_blue = bg_rgb 200 225 245

  let fg_muted_navy = fg_rgb 40 60 90

  (* magenta pastel pair -- bright *)
  let bg_pastel_rose = bg_rgb 245 215 225

  let fg_muted_plum = fg_rgb 90 60 80

  (* red pastel pair -- bright *)
  let bg_pastel_coral = bg_rgb 250 215 210

  let fg_brick_red = fg_rgb 130 60 52
end

(*
TODO [improvements]
Consider improvements:
===========================
1. this module mixes the following, we could split along these responsibilities:
  - low-level ANSI functions
  - high-level named styles
  - pastel custom theme
  - centering/padding logic

2. natural outcome of this is going to be theme defs.

2. annoyance: too many functions defined here.
   possible cleanup: define style variants and use them via a render pipeline of sorts
  - e.g. style:
    type t =
      | Fg of int
      | Bg of int
      | Bold
      | Italic
      | Underline
      | Rgb_fg of int * int * int
      | Rgb_bg of int * int * int

    let render styles =
      let codes =
          List.map styles ~f:(function
            | Bold -> "1"
            | Italic -> "3"
            | Underline -> "4"
            | Fg n -> Printf.sprintf "38;5;%d" n
            | Bg n -> Printf.sprintf "48;5;%d" n
            | Rgb_fg (r,g,b) -> Printf.sprintf "38;2;%d;%d;%d" r g b
            | Rgb_bg (r,g,b) -> Printf.sprintf "48;2;%d;%d;%d" r g b)
      in
      let code = String.concat ~sep:";" codes in
      fun s -> Printf.sprintf "\027[%sm%s\027[0m" code s
*)

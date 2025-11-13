open Base

(** A string example of a value*)
module Value_string : Value.S = struct
  type t = string [@@deriving sexp, compare, equal, hash, yojson]

  let to_string s = s

  type spec = {value: string} [@@deriving sexp, yojson]

  let of_spec {value} : t = value
end

open Base

(** A string example of a value*)
module Value_string : Value.S = struct
  type t = string [@@deriving sexp, compare, equal, hash]
  let to_string s = s
end

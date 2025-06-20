open Syntax
open Format

let pp_value ppf = function
  | IntV i -> pp_print_int ppf i
  | BoolV b -> pp_print_bool ppf b
  | VoidV -> pp_print_string ppf "()"

type id = string

module Store = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
  )

type ty = TyInt | TyBool | TyVoid

type binop = Plus

type value = 
  | IntV of int
  | BoolV of bool
  | VoidV

type expr = 
  | IConst of int
  | BConst of bool
  | Var of id
  | BinOp of binop * expr * expr
  | Out of expr (* will be deleted *)
and command = 
  | Declare of ty * id
  | Substitute of id * expr
  | Exp of expr

type mthd = {
  name : string;
  body : command list;
}

type class_declaration = {
   name : string;
   methods : mthd list;
}

type program = class_declaration list
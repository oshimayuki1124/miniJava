
type id = string

module Store = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
  )

type ty = TyInt | TyBool | TyVoid

type binop = Plus | Minus | Prod | Div | Mod | Lt | Gt | Le | Ge | Eq | Neq | Land | Lor

type value = 
  | IntV of int
  | BoolV of bool
  | VoidV

type exp = 
  | IConst of int
  | BConst of bool
  | Var of id
  | BinOp of binop * exp * exp
  | Out of exp (* will be deleted *)
  | Call of id
and command = 
  | Declare of ty * id
  | Substitute of id * exp
  | If of (exp * command list) list
  | While of exp * command list
  | Exp of exp

type args = (id * ty) list

type mthd = args * ty * command list

type cls = mthd Store.t

type program = cls Store.t
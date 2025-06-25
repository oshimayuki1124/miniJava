exception Void_declaration

type id = string

module Store = Map.Make (
  struct
    type t = id
    let compare (x : id) y = compare x y
  end
  )

type ty = TyInt | TyBool | TyVoid | TyObj of id

type binop = Plus | Minus | Prod | Div | Mod | Lt | Gt | Le | Ge | Eq | Neq | Land | Lor

type exp = 
  | IConst of int
  | BConst of bool
  | Var of id
  | BinOp of binop * exp * exp
  | Out of exp (* will be deleted *)
  | Access_field of id list
  | Call_method of id list * exp list
  | Instanciation of id
and command = 
  | Declare of ty * id
  | Substitute of id * exp
  | If of (exp * command list) list
  | While of exp * command list
  | Return of exp
  | Exp of exp

type value = 
  | IntV of int
  | BoolV of bool
  | VoidV
  | NullV
  | ObjV of id * value Store.t * (id list * command list) Store.t

type args = (id * ty) list

type mthd = args * ty * command list

type cls = (ty * value) Store.t * mthd Store.t

type program = cls Store.t

let initial_val ty = match ty with
  | TyInt -> IntV 0
  | TyBool -> BoolV false 
  | TyVoid -> raise @@ Void_declaration
  | TyObj _ -> NullV
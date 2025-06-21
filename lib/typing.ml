open Syntax

exception Type_error of string

let class_store = ref Store.empty
let current_class = ref ""

let typing_binop op = match op with
  | Plus | Minus | Prod | Div | Mod -> TyInt, TyInt, TyInt
  | Lt | Gt | Le | Ge | Eq | Neq -> TyInt, TyInt, TyBool
  | Land | Lor -> TyBool, TyBool, TyBool 

let rec typing_exp e tystore = match e with
  | IConst _ -> TyInt
  | BConst _ -> TyBool
  | Var id -> 
    begin try Store.find id tystore
    with Not_found -> raise @@ Type_error ("variable " ^ id ^ " is not found") end
  | BinOp (op, e1, e2) ->
    let t1 = typing_exp e1 tystore in
    let t2 = typing_exp e2 tystore in
    let t1', t2', t = typing_binop op in
    if t1 = t1' && t2 = t2' then t
    else raise @@ Type_error "binop"
  | Out e ->
    let t = typing_exp e tystore in
    if t = TyInt || t = TyBool then TyVoid
    else raise @@ Type_error "out"
  | Call (id, es) -> 
    let (args, t, _) = Store.find !current_class !class_store |> Store.find id in
    let formal_arg_tys = List.map (fun (_, t) -> t) args in
    let actual_arg_tys = List.map (fun e -> typing_exp e tystore) es in
    if List.fold_left2 (fun b -> fun fa -> fun aa -> b && fa = aa) true formal_arg_tys actual_arg_tys then t
    else raise @@ Type_error ("calling " ^ id)

let rec typing_command c rt tystore = match c with
  | Declare (t, id) -> Store.add id t tystore
  | Substitute (id, e) ->
    let ty = typing_exp e tystore in
    begin try 
      if Store.find id tystore = ty then tystore
      else raise @@ Type_error ("substitution in " ^ id)
    with Not_found -> raise @@ Type_error ("variable " ^ id ^ " is not found") end
  | If p -> begin match p with
    | (e, cs) :: t -> 
      let ty = typing_exp e tystore in
      if ty = TyBool then (typing_commands cs rt tystore; typing_command (If t) rt tystore)
      else raise @@ Type_error ("if statement")
    | [] -> tystore
    end
  | While (e, cs) ->
    let ty = typing_exp e tystore in
    if ty = TyBool then (typing_commands cs rt tystore; tystore)
    else raise @@ Type_error ("if statement")
  | Return e -> 
    let ty = typing_exp e tystore in
    if ty = rt then tystore
    else raise @@ Type_error ("return type")
  | Exp e -> 
    let _ = typing_exp e tystore in tystore
and typing_commands cs rt tystore = match cs with
  | c :: t -> 
    let tystore = typing_command c rt tystore in
    typing_commands t rt tystore
  | [] -> ()

let rec typing_methods (ms : (id * mthd) list) = match ms with
  | (_, (args, rt, cs)) :: t -> 
    let store = List.fold_left (fun store -> fun (id, ty) -> Store.add id ty store) Store.empty args in
    typing_commands cs rt store;
    typing_methods t
  | [] -> ()

let rec typing_classes (cs : (id * cls) list) = match cs with
  | (id, ms) :: t -> 
    let bindings : (id * mthd) list = Store.bindings ms in
    current_class := id;
    typing_methods bindings; typing_classes t
  | [] -> ()

let typing (cs:program) =
  class_store := cs;
  let bindings = Store.bindings cs in
  typing_classes bindings
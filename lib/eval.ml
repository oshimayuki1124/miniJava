open Syntax
open Format

exception Eval_error of string
exception Eval_bug of string

let classes = ref []

let eval_op op v1 v2 _ = match op, v1, v2 with
  | Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | _ -> raise @@ Eval_bug "eval_op is not defined completely"

let rec eval_classes cs class_name method_name store = match cs with
  | {name=class_name'; methods=ms} :: t ->
    if class_name = class_name' then eval_methods ms method_name store
    else eval_classes t class_name method_name store
  | _ -> raise @@ Eval_error ("class " ^ class_name ^ " did not find")
and eval_methods ms method_name store = match ms with
  | {name=method_name'; body=b} :: t ->
    if method_name = method_name' then eval_body b store
    else eval_methods t method_name store
  | _ -> raise @@ Eval_error ("method " ^ method_name ^ " did not find")
and eval_body b store = match b with
  | h :: t -> eval_body t (eval_command h store)
  | [] -> store
and eval_command c store = match c with
  | Declare (t, id) -> 
    begin try 
      let _ = Store.find id store in raise @@ Eval_error (id ^ "is already declared")
    with Not_found -> begin match t with
      | TyInt -> Store.add id (IntV 0) store
      | TyBool -> Store.add id (BoolV false) store
      | TyVoid -> raise @@ Eval_error "void value should not be declared"
    end end
  | Substitute (id, e) ->
    let v, store = eval_exp e store in
    Store.add id v store
  | Exp e -> let _, store = eval_exp e store in store
and eval_exp e store = match e with
  | IConst i -> IntV i, store
  | BConst b -> BoolV b, store
  | Var id -> 
    begin try Store.find id store, store
    with Not_found -> raise @@ Eval_error (id ^ " is not declared") end
  | BinOp (op, e1, e2) -> 
    let v1, store = eval_exp e1 store in
    let v2, store = eval_exp e2 store in
    eval_op op v1 v2 store, store
  | Out e ->
    let v, store = eval_exp e store in
    fprintf std_formatter "%a\n" Pp.pp_value v;
    VoidV, store

let eval cs class_name method_name = 
  classes := cs;
  eval_classes cs class_name method_name
  
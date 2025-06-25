open Syntax
open Format

exception Eval_error of string
exception Eval_bug of string
exception Ret of value * (value Store.t * value Store.t)

let class_store = ref Store.empty
let current_class = ref ""

let eval_op op v1 v2 = match op, v1, v2 with
  | Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Prod, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Div, IntV i1, IntV i2 -> IntV (i1 / i2)
  | Mod, IntV i1, IntV i2 -> IntV (i1 mod i2)
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Gt, IntV i1, IntV i2 -> BoolV (i1 > i2)
  | Le, IntV i1, IntV i2 -> BoolV (i1 <= i2)
  | Ge, IntV i1, IntV i2 -> BoolV (i1 >= i2)
  | Eq, IntV i1, IntV i2 -> BoolV (i1 = i2)
  | Neq, IntV i1, IntV i2 -> BoolV (i1 <> i2)
  | Land, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | Lor, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | _ -> raise @@ Eval_error "wrong values are given to BinOp"

let rec eval_body b stores = match b with
  | h :: t -> eval_body t (eval_command h stores)
  | [] -> stores
and eval_command c (store, store_field as stores) = match c with
  | Declare (ty, id) -> 
    if Store.mem id store then raise @@ Eval_error (id ^ "is already declared")
    else Store.add id (initial_val ty) store, store_field
  | Substitute (id, e) ->
    let v, (store, store_field) = eval_exp e stores in
    if Store.mem id store then Store.add id v store, store_field
    else if Store.mem id store_field then store, Store.add id v store_field
    else raise @@ Eval_bug (id ^ " is not declared")
  | If p -> begin match p with
    | (e, cs) :: t -> 
      let v, stores = eval_exp e stores in
      begin match v with
        | BoolV true -> eval_body cs stores
        | BoolV false -> eval_command (If t) stores
        | _ -> raise @@ Eval_error "if condition expression is evaled to not boolean value"
      end
    | [] -> stores
    end
  | While (e, cs) as c ->
    let v, stores = eval_exp e stores in
    begin match v with
      | BoolV true -> let stores = eval_body cs stores in eval_command c stores
      | BoolV false -> stores
      | _ -> raise @@ Eval_error "while condition expression is evaled to not boolean value"
    end
  | Return e ->
    let v, stores = eval_exp e stores in
    raise @@ Ret (v, stores)
  | Exp e -> 
    let _, stores = eval_exp e stores in 
    stores
and eval_exp e (store, store_field as stores) = match e with
  | IConst i -> IntV i, stores
  | BConst b -> BoolV b, stores
  | Var id -> 
    begin try Store.find id store, stores
    with Not_found -> try Store.find id store_field, stores
    with Not_found -> raise @@ Eval_error (id ^ " is not declared") end
  | BinOp (op, e1, e2) -> 
    let v1, stores = eval_exp e1 stores in
    let v2, stores = eval_exp e2 stores in
    eval_op op v1 v2, stores
  | Out e ->
    let v, stores = eval_exp e stores in
    fprintf std_formatter "%a\n" Pp.pp_value v;
    VoidV, stores
  | Instanciation id -> 
    let fs, ms = Store.find id !class_store in
    let store_field_obj = Store.map snd fs in
    let store_method_obj = Store.map (fun (l, _, cs) -> List.map (fun (id, _) -> id) l, cs) ms in
    ObjV (id, store_field_obj, store_method_obj), stores
  | Access_field ids ->
    begin match ids with
    | _ :: [] -> raise @@ Eval_bug "This should be considered as Var in parser.mly"
    | id1 :: id2 :: [] -> 
      let objv = try Store.find id1 store with Not_found -> Store.find id1 store_field in
      begin match objv with
        | ObjV (_, store_field_obj, _) -> Store.find id2 store_field_obj, stores
        | _ -> raise @@ Eval_bug (id1 ^ " should be object value")
      end
    | _ -> raise @@ Eval_bug "inner_class yet"
    end
  | Call_method (ids, es) -> 
    match ids with
      | id :: [] -> 
        let (formal_args, _, body) = Store.find !current_class !class_store |> snd |> Store.find id in
        let actual_args_rev, (store, store_field) = List.fold_left (fun (l, s) -> fun e -> let v, s = eval_exp e s in v :: l, s) ([], stores) es in
        let stores = List.fold_left2 (fun store -> fun (id, _) -> fun arg -> Store.add id arg store) Store.empty formal_args (List.rev actual_args_rev), store_field in
        begin try
          let _, store_field = eval_body body stores in VoidV, (store, store_field)
        with
          Ret (v, (_, store_field)) -> v, (store, store_field)
        end
      | id1 :: id2 :: [] -> 
        let objv = try Store.find id1 store with Not_found -> Store.find id1 store_field in
        begin match objv with
          | ObjV (class_id, store_field_obj, store_method_obj) -> 
            let (formal_args, body) = Store.find id2 store_method_obj in
            let actual_args_rev, (store, store_field) = List.fold_left (fun (l, s) -> fun e -> let v, s = eval_exp e s in v :: l, s) ([], stores) es in
            let stores_obj = List.fold_left2 (fun store -> fun id -> fun arg -> Store.add id arg store) Store.empty formal_args (List.rev actual_args_rev), store_field_obj in
            begin try
              let _, store_field_obj = eval_body body stores_obj in 
              let stores = 
                if Store.mem id1 store then Store.add id1 (ObjV (class_id, store_field_obj, store_method_obj)) store, store_field
                else if Store.mem id1 store_field then store, Store.add id1 (ObjV (class_id, store_field_obj, store_method_obj)) store_field
                else raise @@ Eval_bug (id1 ^ " is not declared")
              in VoidV, stores
            with Ret (v, (_, store_field_obj)) -> 
              let stores =  
                if Store.mem id1 store then Store.add id1 (ObjV (class_id, store_field_obj, store_method_obj)) store, store_field
                else if Store.mem id1 store_field then store, Store.add id1 (ObjV (class_id, store_field_obj, store_method_obj)) store_field
                else raise @@ Eval_bug (id1 ^ " is not declared")
              in v, stores
            end
          | _ -> raise @@ Eval_bug (id1 ^ " should be object value")
        end        
      | _ -> raise @@ Eval_bug "inner_class yet"

let eval (cs:program) file_name = 
  class_store := cs;
  current_class := file_name;
  let store_field = Store.find file_name cs |> fst |> Store.map snd in
  let (_, _, body) = Store.find file_name cs |> snd |> Store.find "main" in
  eval_body body (Store.empty, store_field)
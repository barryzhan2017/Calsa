(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)



let empty_func_def = {
  rtyp = Any;
  fname = "";
  formals = [];
  body = [];
}

let to_formals fakes =  List.map extract_svar fakes
let empty_func t = ({ typ_t = t; formals_t = [] })
let concrete_func t f = ({ typ_t = t; formals_t = List.map fst (to_formals f) }) 

let function_to_sfunction typ = match typ with
    Init(typ, assign) -> raise (Failure ("Formals contain init type")) 
  | Decl(typ', id) -> (match typ' with 
      Function ->  Decl(SFunction (empty_func Any), id)
    | _ -> Decl(typ', id) )

(* Convert the original abstract declaration
   to variable declaration or function declaration
   according to their type *)
let rec convert (defs, globals, functions) = 
  match defs with
  | [] -> (List.rev globals, List.rev functions)
  | def::tails ->
    match def with
      Ast.VarDef var_def -> convert (tails, var_def::globals, functions)
    | Ast.FuncDef func_def -> convert (tails, globals , func_def::functions)

(* Add a variable to symbol *)
(* Add case for function declaration for return type match *)
let add_symbol m var = 
  match var with
    Decl (typ, name) ->m:= 
    if typ = Function then 
    StringMap.add name (SFunction (empty_func Any)) !m 
    else StringMap.add name typ !m 
    ;m
  | Init(typ, assign) -> m:= 
    if typ = Function  then (match (snd assign) 
    with FuncExpr(expr) -> StringMap.add (fst assign) (SFunction (empty_func expr.rtyp)) !m
    | _ -> raise (Failure ("Function init by none function expression!"))) 
    else StringMap.add (fst assign) typ !m
    ;m

(* Return a variable from symbol table *)
let type_of_identifier s symbols=
  try StringMap.find s !symbols
  with Not_found -> raise (Failure ("undeclared identifier " ^ s))

(* Return a function from our symbol table *)
let find_func s funcs =
  try StringMap.find s funcs
  with Not_found -> raise (Failure ("unrecognized function " ^ s))
 

(* Find the function in the symbol table firstly and try to find it in declared global functions*)
let find_func_symbols s symbols funcs =
  try let typ = StringMap.find s !symbols 
  in (match typ with
  SFunction(f) -> empty_func_def
  |_ -> raise (Failure ("unrecognized function " ^ s)))
  with Not_found -> find_func s funcs


let rec check_array_type v = 
  let h = List.hd v in
  if List.length v = 1 then (fst h, [h])
  else
    let ty1 = fst h
    and (ty2, es) = check_array_type (List.tl v) in
    if ty1 = ty2 then (ty1, h::es)
    else raise (Failure ("Inconsist Type"))

(* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type. Add cases for print statment and call having function variable*)
let check_assign lvaluet rvaluet err =
  if lvaluet = rvaluet || lvaluet = Any || lvaluet = SFunction (empty_func Any) then rvaluet else raise (Failure err)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.
   Check each global variable, then check each function *)

let check (defs) = 
  let globals, functions = convert (defs, [], []) 
  in
  let global_symbols = List.fold_left add_symbol (ref StringMap.empty) globals
  in
  (* Convert Function to SFuntion in formals for symbol table lookup*)
  let functions' = List.map (fun f -> 
  let formals' = List.map (fun formal -> (function_to_sfunction formal)) f.formals
  in
  ({
    rtyp = f.rtyp;
    fname = f.fname;
    formals = formals';
    body = f.body;
  })) functions
  in
  (* Verify a list of bindings has no duplicate names *)
  let check_duplicates (kind : string) (binds : var_def list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) (List.map Ast.extract_var binds))
  in

  (* Make sure no globals duplicate *)
  check_duplicates "global" globals;

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (function_name, param_type, return_type) = StringMap.add function_name {
        rtyp = return_type;
        fname = function_name;
        formals = List.map (fun t -> Decl (t, "x")) param_type;
        body = [] } map
    in List.fold_left add_bind StringMap.empty 
      [("print", [Any], Int); 
      ("add", [List; Int], Bool); ("get", [Any; Int], Int); ("remove", [List; Int], Bool); ("set", [Any; Int; Int], Bool); ("insert", [List; Int; Int], Bool); ("size", [Any], Int); ("sum", [List], Int); 
      ("hasKey", [Hashtable; Int], Bool)] in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions'
  in

  let _ = find_func "main" function_decls in (* Ensure "main" is defined *)
  (* Return a semantically-checked expression, i.e., with a type *)
  let rec check_expr ( symbols:'a StringMap.t ref ) (funcs:Ast.func_def StringMap.t) expr =
    match expr with
      Literal l -> (Int, SLiteral l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | ArrayLit l -> 
      let (ty, e) = check_array_type (List.map (check_expr symbols funcs) l) in
      (Array(ty, List.length e), SArrayLit e)
    | Id var -> (type_of_identifier var symbols, SId var)
    | ArrayAccess(var, idx) -> 
      (
        let ty = (type_of_identifier var symbols) in
        match ty with
          Array(ty', len) ->
          if idx >= 0 && idx < len then (ty', SArrayAccess(var, idx))
          else raise (Failure ("Invalid index"))
        | _ -> raise (Failure ("Inconsist Type"))
      )
    | StringLit s -> (String, SStringLit s)
    | Assign(var, e) as ex ->
      let lt = type_of_identifier var symbols
      and (rt, e') = check_expr symbols funcs e in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))
    | ArrayAssign(var, i, e) as ex ->
      (
        let lt = type_of_identifier var  symbols in
        match lt with
          Array (t, len) -> (
            let (rt, e') = check_expr symbols funcs e in
            if len > i then
              let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                        string_of_typ rt ^ " in " ^ string_of_expr ex
              in
              (check_assign t rt err, SArrayAssign(var, i, (rt, e')))
            else
              raise (Failure ("Index out of range "))
          )
        | _ -> raise (Failure ("Unexpected Type"))
      )
    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_expr symbols funcs e1 
      and (t2, e2') = check_expr symbols funcs e2 in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub | Mod | Mul | Div when t1 = Int -> Int
          | Add | Sub | Mod | Mul | Div when t1 = Float -> Float
          | Equal | Neq -> Bool
          | Less when t1 = Int -> Bool
          | Less when t1 = Float -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      else raise (Failure err)
      (* If the function call is come from function variable, ignore the prototype checking*)
    | Call(fname, args) as call -> (
      let fd = find_func_symbols fname symbols funcs in
      match fd.rtyp with 
        Any -> let args' = List.map (check_expr symbols funcs) args in 
        (fd.rtyp, SCall(fname, args'))
      | _ ->
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^
                        " arguments in " ^ string_of_expr call))
      else let check_call (ft, _) e =
            let (et, e') = check_expr symbols funcs e in
            let err = "illegal argument found " ^ string_of_typ et ^
                      " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
        in
        let args' = List.map2 check_call (List.map Ast.extract_var fd.formals) args
        in (fd.rtyp, SCall(fname, args')))
      (* Fuction expression: Evaluate the function recursively and give the type by created function expression*)
    | FuncExpr fn -> 
      let fuc_expr = check_func symbols fn
      in 
      (SFunction (concrete_func fuc_expr.srtyp fuc_expr.sformals), SFuncExpr fuc_expr)

  (* Add: convert function type to sfunction type *)
  (* Add assignment check *)
  and check_var symbols funcs var = 
    match var with
      Decl(typ, name) -> (match typ with 
        Function-> SDecl(SFunction (empty_func Any), name)
      | _ -> SDecl(typ, name))
    | Init(ltyp, assign) -> let to_sassign = function
          (string, expr) -> (string, (check_expr symbols funcs expr))
      in let sassign = (to_sassign assign) in 
        let rtyp = fst (snd sassign)
      in     
        let err = "illegal initialization " ^ string_of_typ ltyp ^ " = " ^
      string_of_typ rtyp ^ " in " ^ string_of_expr (snd assign)
      in
      (match ltyp with 
        Function-> SInit(check_assign (SFunction (empty_func Any)) rtyp err, sassign)
      | _ -> SInit(check_assign ltyp rtyp err, sassign))
    (*add table to support nested functions' symbols lookup*)
  and check_func table func  =
    (* Make sure no formals or locals are void or duplicates *)
    check_duplicates "formal" func.formals;

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left add_symbol
        table (globals @ func.formals)
    in

    let check_bool_expr e =
      let (t, e') = check_expr symbols function_decls e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    (* let rec check_stmt_list ( symbols:'a StringMap.t ref ) =function
        [] -> []
       | Block sl :: sl'  -> check_stmt_list symbols (sl @ sl') (* Flatten blocks *)
       | s :: sl -> (check_stmt symbols s):: check_stmt_list symbols sl *)
    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt symbols = function
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (List.map (check_stmt symbols) sl)
      | LocalVarDef (v) ->  ignore(add_symbol symbols v);
        SLocalVarDef(check_var symbols function_decls v)
      | Expr e -> SExpr (check_expr symbols function_decls e )
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt symbols st1, check_stmt symbols st2)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt symbols st)
        (*Add further check if the returned variable is a function variable, we should check the return of the function*)
      | Return e ->
        let (t, e') = check_expr symbols function_decls e in
        let t' = match t with 
          SFunction(f) -> f.typ_t
          | _ -> t
        in 
        (* Add any type case for call expression using a function variable *)
        if t' = func.rtyp || t' = Any then SReturn (func.rtyp, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t' ^ " expected " ^
                     string_of_typ func.rtyp ^ " in " ^ string_of_expr e))


    in (* body of check_func *)
    { 
      srtyp = func.rtyp;
      sfname = func.fname;
      sformals = List.map (check_var symbols function_decls) func.formals;
      sbody = List.map (check_stmt symbols) func.body
    }
  in
  let globals = List.map (fun x-> Sast.SVarDef(x)) (List.map (check_var global_symbols function_decls) globals)  in
  let functions = List.map (fun x->Sast.SFuncDef(x)) (List.map (check_func (ref StringMap.empty)) functions ) in
  (globals, functions)
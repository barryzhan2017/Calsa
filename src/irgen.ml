(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Lambda
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions: Sast.svar_def list * (string * Lambda.lfexpr) list) =

  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "MicroC" in


  (* helper functions *)
  let rec generate_seq n = 
    let rec generate_rev_seq n = if n >= 0 then (n :: generate_rev_seq(n-1)) else [] in
    List.rev (generate_rev_seq n) in

  let build_struct builder agg value idx = 
    L.build_insertvalue agg value idx "tmp" builder in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and void_t     = L.void_type   context
  and float_t    = L.double_type context
  and i1_t       = L.i1_type     context 
  and string_t   = L.pointer_type (L.i8_type context)
  and void_ptr_t = L.pointer_type (L.i8_type context)
  in

  (* Declare struct StringList *)

  let struct_listNode_t : L.lltype =
    L.named_struct_type context "ListNode" in
  let struct_list_t : L.lltype =
    L.named_struct_type context "List" in
  let _ =
    L.struct_set_body struct_list_t
      [| L.pointer_type struct_listNode_t; L.pointer_type struct_listNode_t; i32_t |] false in

  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Void  -> void_t
    | A.Float -> float_t
    | A.Bool  -> i1_t
    | A.String -> string_t
    | A.Array(t, len) -> L.array_type (ltype_of_typ t) len
    | A.List -> struct_list_t
    | A.SFunction f-> ltype_of_sfunc_t_clsr "" f
    | _ -> raise(Failure"Not implemented yet!")

  (* llvm type of lfexpr *)
  and ltype_of_lfexpr (name:string) (lfexpr:Lambda.lfexpr) = 
    let f = (List.map (fun (t, _) -> ltype_of_typ t) lfexpr.lformals)
    in let formal_types = if (name = "main") then f else void_ptr_t::f
    in L.function_type (ltype_of_typ lfexpr.ltyp) (Array.of_list formal_types)

  (* llvm type of SFunction *)
  and ltype_of_sfunc_t (name:string) (sfunc_t:A.sfunc_t) = 
    let f = List.map ltype_of_typ sfunc_t.formals_t in
    let formal_types = if (name = "main") then f else void_ptr_t::f
    in L.function_type (ltype_of_typ sfunc_t.typ_t) (Array.of_list formal_types)

  (* llvm type of closure of lfexpr *)
  and ltype_of_sfunc_def_clsr (name:string) (lfexpr:Lambda.lfexpr) = 
    let func_t = L.pointer_type (ltype_of_lfexpr name lfexpr)
    in L.struct_type context [|func_t;void_ptr_t|]

  (* llvm type of closure of SFunction *)
  and ltype_of_sfunc_t_clsr (name:string) (sfunc_t:A.sfunc_t) = 
    let func_t = L.pointer_type (ltype_of_sfunc_t name sfunc_t)
    in L.struct_type context [|func_t;void_ptr_t|]

  in

  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| string_t |] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Declare each C++ function *)
  let add_t : L.lltype = L.function_type i1_t [| L.pointer_type struct_list_t; i32_t |] in
  let add_func : L.llvalue = L.declare_function "add" add_t the_module in

  let initList_t : L.lltype = L.function_type void_t [| L.pointer_type struct_list_t |] in
  let initList_func : L.llvalue = L.declare_function "initList" initList_t the_module in

  let printList_func : L.llvalue = L.declare_function "print" initList_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * lfexpr) StringMap.t =
    let function_decl m (name, lfexpr) =
      let ftype = ltype_of_lfexpr name lfexpr in 
      StringMap.add name (L.define_function name ftype the_module, lfexpr) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body (name, lfexpr: StringMap.key * Lambda.lfexpr) =
    let (the_function, _) = StringMap.find name function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (* Create c style print format *)
    let rec format_type = function
        A.Int   -> "%d"
      | A.Float -> "%.2f"
      | A.Bool  -> "%d"
      | A.String -> "%s"
      | A.Any -> raise (Failure ("Not implemented yet!"))
      | A.List -> raise (Failure ("Not implemented yet!")) 
      | A.Void ->  raise (Failure ("Not implemented yet!")) 
      | A.Array(t, len) -> 
        let format =  Array.make len (format_type t) in 
        let result = Array.fold_left (fun a b -> a ^ b ^ ", ") "[" format in
        (String.sub result 0 (String.length result - 2)) ^ "]"
      | _ -> raise(Failure("Cannot print a function.")) in

    let format_str t = L.build_global_stringptr ((format_type t) ^ "\n") "fmt" builder in

    let lookup_value local_vars global_vars n = 
      let (_,value) = try StringMap.find n !local_vars
        with Not_found -> StringMap.find n !global_vars  in value
    in
    let lookup local_vars global_vars n = 
      try StringMap.find n !local_vars
      with Not_found -> StringMap.find n !global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec build_expr builder local_vars global_vars ((_, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i
      | SFloatLit f -> L.const_float float_t f
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SArrayLit a -> let (ty, sx) = (List.hd a) in
        L.const_array (ltype_of_typ ty) (Array.of_list (List.map (build_expr builder local_vars global_vars) a))
      | SArrayAccess (s, i) -> (*L.build_extractvalue (L.build_load (lookup v) v builder) i "" builder*)
        let ptr = L.build_struct_gep (lookup_value local_vars global_vars s) i "addr" builder in
        L.build_load ptr s builder

      | SId s       -> L.build_load (lookup_value local_vars global_vars s ) s builder

      | SStringLit s -> L.build_global_stringptr s "" builder

      | SAssign (s, e) -> let e' = build_expr builder local_vars global_vars e in
        ignore(L.build_store e' (lookup_value local_vars global_vars s) builder); e'
      | SArrayAssign (s, i, e) -> 
        let e' = build_expr builder local_vars global_vars e
        and ptr = L.build_struct_gep (lookup_value local_vars global_vars s) i "addr" builder in
        ignore(L.build_store e' ptr builder); e'
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder local_vars global_vars e1 
        and e2' = build_expr builder local_vars global_vars e2 in
        (match op with
           A.Add     -> (match e1, e2 with 
               (A.Int, _), (A.Int, _)  ->  L.build_add
             | (A.Float, _), (A.Float, _)  ->  L.build_fadd
             | _ -> raise (Failure ("Mix of float and int not supported"))
           )
         | A.Sub     -> (match e1, e2 with 
               (A.Int, _), (A.Int, _)  ->  L.build_sub
             | (A.Float, _), (A.Float, _)  ->  L.build_fsub
             | _ -> raise (Failure ("Mix of float and int not supported"))
           )
         | A.Mul     -> (match e1, e2 with 
               (A.Int, _), (A.Int, _)  ->  L.build_mul
             | (A.Float, _), (A.Float, _)  ->  L.build_fmul
             | _ -> raise (Failure ("Mix of float and int not supported"))
           )
         | A.Div     -> (match e1, e2 with 
               (A.Int, _), (A.Int, _)  ->  L.build_sdiv
             | (A.Float, _), (A.Float, _)  ->  L.build_fdiv
             | _ -> raise (Failure ("Mix of float and int not supported"))
           )  (* add nsw for overflow detection? *)
         | A.Mod     -> (match e1, e2 with 
               (A.Int, _), (A.Int, _)  ->  L.build_srem
             | (A.Float, _), (A.Float, _)  ->  L.build_frem
             | _ -> raise (Failure ("Mix of float and int not supported"))
           ) 
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> (match e1, e2 with 
               (A.Int, _), (A.Int, _)  ->  L.build_icmp L.Icmp.Eq
             | (A.Float, _), (A.Float, _)  ->  L.build_fcmp L.Fcmp.Oeq
             | _ -> raise (Failure ("Mix of float and int not supported"))
           )
         | A.Neq     -> (match e1, e2 with 
               (A.Int, _), (A.Int, _)  ->  L.build_icmp L.Icmp.Ne
             | (A.Float, _), (A.Float, _)  ->  L.build_fcmp L.Fcmp.One
             | _ -> raise (Failure ("Mix of float and int not supported"))
           )
         | A.Less    -> (match e1, e2 with 
               (A.Int, _), (A.Int, _)  ->  L.build_icmp L.Icmp.Slt
             | (A.Float, _), (A.Float, _)  ->  L.build_fcmp L.Fcmp.Olt
             | _ -> raise (Failure ("Mix of float and int not supported"))
           )
        ) e1' e2' "tmp" builder
      | SCall ("print", [(t, e)]) ->
        if t = List then
          match e with
          | SId s->
            L.build_call printList_func [| L.build_bitcast (L.build_struct_gep (lookup_value local_vars global_vars s) 0 "" builder) (L.pointer_type struct_list_t) "" builder |] "" builder
          | _ ->raise (Failure"Print a list using its name")
        else
          L.build_call printf_func [| format_str t; (build_expr builder local_vars global_vars (t, e)) |]
            "printf" builder
      | SCall ("add", [(t1, SId s); (t2, e2)]) ->
        L.build_call add_func [| L.build_bitcast (L.build_struct_gep (lookup_value local_vars global_vars s) 0 "" builder) (L.pointer_type struct_list_t) "" builder; (build_expr builder local_vars global_vars (t2, e2)) |]
          (*Array.of_list (List.map (build_expr builder local_vars global_vars) args)*)
          "add" builder
      | SCall (f, args) ->
        let (typ, lclsr) = lookup local_vars global_vars f in
        let func_t = match typ with A.SFunction func_t -> func_t
                                  | _-> raise(Failure"Wrong type for function call!") in
        let clsr_value = L.build_load lclsr "clsr_value" builder in
        (* extract function pointer from lclsr *)
        let func_ptr = L.build_extractvalue clsr_value 0 "func_ptr" builder in
        (* extract environment pointer from lclsr *)
        let env_ptr = L.build_extractvalue clsr_value 1 "env_ptr" builder in
        let llargs = env_ptr :: List.rev (List.map (build_expr builder local_vars global_vars) (List.rev args)) in
        let result = match func_t.typ_t with
            A.Void -> ""
          | _ -> f ^ "_result" in
        L.build_call func_ptr (Array.of_list llargs) result builder 

      | SClosure clsr -> 
        let fvs = List.map snd clsr.fvs in
        (* get values of free variables from local and global variables *)
        let llfvs = List.map (lookup_value local_vars global_vars) fvs in
        let fvs_t = List.map ltype_of_typ (List.map fst clsr.fvs) in
        let fvs_ptr_t = List.map L.pointer_type fvs_t in
        let env_strcut_t = L.struct_type context (Array.of_list fvs_ptr_t) in
        let env_struct = L.build_malloc env_strcut_t "env" builder in
        let idx = generate_seq((List.length fvs)-1) in
        let env_value = List.fold_left2 (build_struct builder) (L.const_null env_strcut_t) 
            llfvs idx in 
        let _ = L.build_store env_value env_struct builder in
        let env_struct_ptr = L.build_bitcast env_struct void_ptr_t "env_ptr" builder in

        let func_name = "f" ^ string_of_int clsr.ind in
        let (func, lfexpr) = StringMap.find func_name function_decls in
        let clsr_struct_t = ltype_of_sfunc_def_clsr func_name lfexpr in 
        let clsr = List.fold_left2 (build_struct builder) (L.const_null clsr_struct_t)
            [func;env_struct_ptr] [0;1] in clsr

      | SFuncExpr sfunc -> raise(Failure"SFuncExpr shoule be converted to SClosure.")

    in
    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    (* Create a map of global variables after creating each *)
    let global_vars =
      let global_var m global =
        match global with
        | SDecl(t,n)->let init = L.const_int (ltype_of_typ t) 0 
          in m:=StringMap.add n (t, L.define_global n init the_module) !m;m
        | SInit(t, sassign)-> let init =  (build_expr builder m (ref StringMap.empty) (snd sassign))
          in m:=StringMap.add (fst sassign) (t, L.define_global (fst sassign) init the_module) !m;m
      in
      List.fold_left global_var (ref StringMap.empty) globals in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        m:=StringMap.add n (t, local) !m;m
      in

      let parameters = Array.to_list (L.params the_function) in
      let formals = if List.length parameters <= 1 then ref StringMap.empty
        else List.fold_left2 add_formal (ref StringMap.empty) lfexpr.lformals (List.tl parameters) in

      let env = if List.length parameters = 0 then L.const_null void_ptr_t else List.hd parameters in 
      let _ = L.set_value_name "env" env in
      let env_void_ptr = L.build_alloca void_ptr_t "env" builder in
      let _ = L.build_store env env_void_ptr builder in
      let env_void = L.build_load env_void_ptr "env_void" builder in

      let expanded_formals = match name with
          "main" -> formals
        | _ -> match lfexpr.lfvs with 
            [] -> formals
          | _ -> let env_struct = L.struct_type context 
                     (Array.of_list (List.map (fun (t,_)-> L.pointer_type (ltype_of_typ t)) lfexpr.lfvs)) in 
            let env_ptr_t = L.pointer_type env_struct in
            let env_ptr = L.build_bitcast env_void env_ptr_t "env_p" builder in 
            let env_val = L.build_load env_ptr "env_val" builder in 
            let init_free_var m (t, n) idx = 
              let free_var = L.build_extractvalue env_val idx "tmp_" builder in 
              m:=StringMap.add n (t, free_var) !m;m in 
            List.fold_left2 init_free_var formals lfexpr.lfvs (generate_seq ((List.length lfexpr.lfvs)-1)) in

      (* Closure of the_function to support recusion call of itself *)
      let clsr_t = ltype_of_sfunc_def_clsr name lfexpr in 
      let clsr_p = L.build_alloca clsr_t name builder in 
      let clsr_val = List.fold_left2 (build_struct builder) (L.const_null clsr_t) [the_function;env_void] [0;1] in 
      let _ = L.build_store clsr_val clsr_p builder in 
      let func_t = A.SFunction ({typ_t = lfexpr.ltyp;formals_t=List.map fst lfexpr.lformals}) in 
      expanded_formals:=StringMap.add name (func_t, clsr_p) !expanded_formals;expanded_formals in

    (* Allocate space for any locally declared variables and add the
       resulting registers to our map *)
    let add_local m local builder =
      match local with
      | SDecl(t, n)->
        let local_var = L.build_alloca (ltype_of_typ t) n builder in
        let _ = 
          (
            match t with
            | A.List -> L.build_call initList_func [| local_var |] "" builder;
            | _ -> local_var
          )
        in m:=StringMap.add n (t, local_var) !m
      | SInit(t, (n, e))->
        let alloca_clsr clsr = 
          let func_name = "f" ^ string_of_int clsr.ind in
          let (_, lfexpr) = StringMap.find func_name function_decls in 
          let func_t = L.pointer_type (ltype_of_lfexpr func_name lfexpr) in 
          let clsr_struct_t = L.struct_type context [|func_t;void_ptr_t|] in 
          L.build_alloca clsr_struct_t n builder in
        let (_, expr) = e in
        let local_var = match expr with
          | SClosure clsr -> alloca_clsr clsr
          | _ -> L.build_alloca (ltype_of_typ t) n builder
        in
        let e' = build_expr builder m global_vars e in
        ignore(L.build_store e' local_var builder);
        m:=StringMap.add n (t, local_var) !m in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder local_vars global_vars e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder local_vars global_vars e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder local_vars global_vars predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder local_vars global_vars predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

      | SLocalVarDef(local) -> 
        add_local local_vars local builder; builder

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock lfexpr.lbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))
  in

  List.iter build_function_body functions;
  the_module

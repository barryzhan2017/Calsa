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
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (defs) =
  let context    = L.global_context () in

  let rec convert (defs, globals, functions) = 
    match defs with
    | [] -> (List.rev globals, List.rev functions)
    | def::tails ->
      match def with
        Sast.SVarDef var_def -> convert (tails, var_def::globals, functions)
      | Sast.SFuncDef func_def -> convert (tails, globals , func_def::functions) in
  let globals, functions = convert (defs, [], []) in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "MicroC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and void_t     = L.void_type   context
  and float_t    = L.double_type context
  and i1_t       = L.i1_type     context 
  and string_t   = L.pointer_type (L.i8_type context)
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
    | A.Any -> raise (Failure ("Not implemented yet!"))
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
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) (List.map Sast.extract_svar fdecl.sformals))
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
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
        (String.sub result 0 (String.length result - 2)) ^ "]" in


    let format_str t = L.build_global_stringptr ((format_type t) ^ "\n") "fmt" builder in

    let lookup n local_vars global_vars = try StringMap.find n !local_vars
      with Not_found -> StringMap.find n !global_vars  in


    (* Construct code for an expression; return its value *)
    let rec build_expr builder local_vars global_vars ((_, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i
      | SFloatLit f -> L.const_float float_t f
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SArrayLit a -> let (ty, sx) = (List.hd a) in
        L.const_array (ltype_of_typ ty) (Array.of_list (List.map (build_expr builder local_vars global_vars) a))
      | SArrayAccess (s, i) -> (*L.build_extractvalue (L.build_load (lookup v) v builder) i "" builder*)
        let ptr = L.build_struct_gep (lookup s local_vars global_vars) i "addr" builder in
        L.build_load ptr s builder

      | SId s       -> L.build_load (lookup s local_vars global_vars) s builder

      | SStringLit s -> L.build_global_stringptr s "" builder

      | SAssign (s, e) -> let e' = build_expr builder local_vars global_vars e in
        ignore(L.build_store e' (lookup s local_vars global_vars) builder); e'
      | SArrayAssign (s, i, e) -> 
        let e' = build_expr builder local_vars global_vars e
        and ptr = L.build_struct_gep (lookup s local_vars global_vars) i "addr" builder in
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
            L.build_call printList_func [| L.build_bitcast (L.build_struct_gep (lookup s local_vars global_vars) 0 "" builder) (L.pointer_type struct_list_t) "" builder |] "" builder
          | _ ->raise (Failure"Print a list using its name")
        else
          L.build_call printf_func [| format_str t; (build_expr builder local_vars global_vars (t, e)) |]
            "printf" builder
      | SCall ("add", [(t1, SId s); (t2, e2)]) ->
        L.build_call add_func [| L.build_bitcast (L.build_struct_gep (lookup s local_vars global_vars) 0 "" builder) (L.pointer_type struct_list_t) "" builder; (build_expr builder local_vars global_vars (t2, e2)) |]
          (*Array.of_list (List.map (build_expr builder local_vars global_vars) args)*)
          "add" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder local_vars global_vars) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder in
    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    (* Create a map of global variables after creating each *)
    let global_vars : L.llvalue StringMap.t ref =
      let global_var m global =
        match global with
        | SDecl(t,n)->let init = L.const_int (ltype_of_typ t) 0 
          in m:=StringMap.add n (L.define_global n init the_module) !m;m
        | SInit(t, sassign)-> let init =  (build_expr builder m (ref StringMap.empty) (snd sassign))
          in m:=StringMap.add (fst sassign) (L.define_global (fst sassign) init the_module) !m;m
      in
      List.fold_left global_var (ref StringMap.empty) globals in


    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m formal p =
        match formal with
        | SDecl(t, n)->
          L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
          m:=StringMap.add n local !m;m
        | SInit(t, assign) -> raise (Failure ("Formal doesn't support initialization"))
      in

      List.fold_left2 add_formal (ref StringMap.empty) fdecl.sformals
        (Array.to_list (L.params the_function)) in

    (* Allocate space for any locally declared variables and add the
      * resulting registers to our map *)
    let add_local m local =
      match local with
      | SDecl(t, n)->
        let ty = ltype_of_typ t in
        let local_var = L.build_alloca ty n builder in
        let _ = 
          (
            match t with
            | A.List -> L.build_call initList_func [| local_var |] "" builder;
            | _ -> local_var
          )
        in m:=StringMap.add n local_var !m;m
      | SInit(t, assign)->
        let local_var = L.build_alloca (ltype_of_typ t) (fst assign) builder in
        let e' = build_expr builder m global_vars (snd assign) in
        ignore(L.build_store e' local_var builder);
        m:=StringMap.add (fst assign) local_var !m;m
    in


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

      | SLocalVarDef(local) -> ignore(add_local local_vars local); builder

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in

  List.iter build_function_body functions;
  the_module

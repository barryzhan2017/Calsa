 open Sast
 open Ast
 
 module StringMap = Map.Make(String)

let f_id = ref (-1)
let idx = ref (-1)

let extract_svar  = function
  SDecl (typ, s) -> (typ, s)
| SInit(typ, sassign) -> (typ, fst sassign)

let to_formals fakes =  List.map extract_svar fakes

let find_and_replace m (t, s) = if (StringMap.mem s m) 
  then (StringMap.find s m, s)
  else (t, s)

 (* Some types for lifting *)
 type environment = {
     variables : typ StringMap.t;
     parent: environment option;
}

type lfexpr = {
     lname : string;
     lfvs : bind list;
     ltyp : typ;
     lformals : bind list;
     lbody : sstmt list;
 }


 let empty_func t = ({ typ_t = t; formals_t = [] }) 
 let concrete_func t f = ({ typ_t = t; formals_t = List.map fst (to_formals f) }) 
 let concrete_func2 t f = ({ typ_t = t; formals_t = List.map fst (f) }) 

 let built_in_decls =
     let add_default map (name, ty) = StringMap.add name (SFunction ty) map
     in List.fold_left add_default StringMap.empty [("print", empty_func Int); ("add", empty_func Bool)]

 (* Look up function: traverse up the tree until
 * we encounter a symbol *)
 let rec lookup (e : environment) name =
 try
   StringMap.find name e.variables
 with Not_found ->
   match e.parent with
       Some(parent) -> lookup parent name
  |    None         -> try StringMap.find name built_in_decls
                       with Not_found -> raise
                             (Failure ("Lambda: undeclared identifier " ^ name))

 let rec dfs_stmt fncs env stmt ref_map = 
   let (fncs', fvs', env', stmt', ref_map') =
     match stmt with
     SBlock(stmts) ->
             let (fncs1, fvs1, _, stmts', ref_map') = dfs_stmts fncs env stmts ref_map
             in (fncs1, fvs1, env, SBlock(stmts'), ref_map') 
   | SLocalVarDef(def) -> (
                    match def with   
                    SInit(t, sass) -> 
                    let (fncs1, fvs1, e1', ref_map') = dfs_expr fncs env (snd sass) ref_map ~fname:(fst sass)  in
                      let (t1, _) = e1' in
                      let new_typ = match (t, t1) with
                                    SFunction(_), SFunction(_) -> t1
                                  | _ -> t
                      in
                      let ref_map'' = match t1 with
                                       Any -> (match e1' with 
                                        (t, SCall(s1, expr)) -> StringMap.add s1 (SFunction (concrete_func2 t expr)) ref_map'
                                        | _  -> raise (Failure ("Lambda: Any type is for " ^ string_of_sexpr e1' ^ "instead of SCall" ))) 
                                     | _ -> ref_map'
                      in
                      let new_env = {variables = StringMap.add (fst sass) new_typ env.variables;
                      parent = env.parent} in
                      let init = SInit(new_typ, ((fst sass), e1')) 
                      in
                      (fncs1, fvs1, new_env, SLocalVarDef(init), ref_map'')
                  | SDecl (t, s) -> 
                      let new_env = {variables = StringMap.add (s) t env.variables;
                      parent = env.parent} 
                      in (fncs, [], new_env, SLocalVarDef(def), ref_map))
   | SExpr(e) ->
                   let (fncs1, fvs1, e1', ref_map') = dfs_expr fncs env e ref_map in
                   (fncs1, fvs1, env, SExpr(e1'), ref_map')
   | SReturn(e) ->
                   let (fncs1, fvs1, e1', ref_map') = dfs_expr fncs env e ref_map in
                   let ref_map'' = (match e1' with 
                     (t, SCall(s1, expr)) -> StringMap.add s1 (SFunction (concrete_func2 t expr)) ref_map'
                   | _ -> ref_map')
                   in
                   (fncs1, fvs1, env, SReturn(e1'), ref_map'')
   | SIf(e, s1, s2) ->
              let (fncs1, fvs1, e', ref_map1) = dfs_expr fncs env e ref_map in
              let (fncs2, fvs2, _, s1', ref_map2) = dfs_stmt fncs1 env s1 ref_map1 in
              let (fncs3, fvs3, _, s2', ref_map3) = dfs_stmt fncs2 env s2 ref_map2 in
              (fncs3, List.concat [fvs1; fvs2; fvs3], env, SIf(e', s1', s2'), ref_map3)
   | SWhile(e, s) ->
              let (fncs1, fvs1, e', ref_map1) = dfs_expr fncs env e ref_map in
              let (fncs2, fvs2, _, s', ref_map') = dfs_stmt fncs1 env s ref_map1 in
              (fncs2, List.concat [fvs1; fvs2], env, SWhile(e', s'), ref_map')
   in
   let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
   let fvs'' = List.filter check_scope fvs' in
   (fncs', fvs'', env', stmt', ref_map')
 and dfs_stmts fncs env stmts ref_map = match stmts with
      stmt :: the_rest ->
              let (fncs1, fvs1, env1, stmts1, ref_map1) = dfs_stmt fncs env stmt ref_map in
              let add_bind m (t, id) = StringMap.add id t m in
              let new_env = { variables = List.fold_left add_bind env1.variables
              fvs1;
                              parent = env1.parent;
              } in
              let (fncs2, fvs2, env2, stmts2, ref_map2) = dfs_stmts fncs1 new_env the_rest ref_map1 in
              (fncs2, List.concat [fvs1; fvs2], env2, stmts1 :: stmts2, ref_map2)
    | [] -> (fncs, [], env, stmts, ref_map)
 and dfs_expr ?fname fncs env e ref_map =
     let t = fst e in
     let expr = snd e in
     let  (fncs', fvs', expr', ref_map') = match expr with
     SCall(s1, exprs) ->
              let fv' =
                      if (StringMap.mem s1 env.variables ||
                          StringMap.mem s1 built_in_decls)
                      then
                          None
                      else
                          Some(lookup env s1, s1)
              in
              let (fncs1, fvs1, exprs', ref_map') = dfs_exprs fncs env exprs ref_map
              in let fvs' = match fv' with
                 Some(x) -> x :: fvs1
              |  _ -> fvs1
              in (fncs1, fvs', (t, SCall(s1, exprs')), ref_map')
   | SAssign(s1, e1) ->
              let (fncs1, fvs1, e1', ref_map') = dfs_expr fncs env e1 ref_map in
              (fncs1, fvs1, (t, SAssign(s1, e1')), ref_map')
   | SArrayAssign(s1, idx, e1) -> 
              let (fncs1, fvs1, e1', ref_map') = dfs_expr fncs env e1 ref_map in
              (fncs1, fvs1, (t, SArrayAssign(s1, idx, e1')), ref_map')
   | SId(s1) ->
              let fv =if (StringMap.mem s1 env.variables ||
                          StringMap.mem s1 built_in_decls)
                      then
                          []
                      else
                          [lookup env s1, s1]
              in
              (fncs, fv, (t, SId(s1)), ref_map)
   | SFuncExpr(e1) ->
              let (fncs1, fvs1, (t, clsr), ref_map') = match fname with Some x -> build_closure
                                                              fncs env e1  ~fname:x ref_map
                                                            | None -> build_closure
                                                              fncs env e1 ref_map
              in
              let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
              let fvs1' = List.filter check_scope fvs1 in
              (fncs1, fvs1', (t, SClosure(clsr)), ref_map')
   | SBinop(e1, op, e2) -> 
              let (fncs1, fvs1, e1', ref_map1) = dfs_expr fncs env e1 ref_map in
              let (fncs2, fvs2, e2', ref_map2) = dfs_expr fncs env e2 ref_map1 in
              (fncs1@fncs2, fvs1@fvs2, (t, SBinop(e1',op, e2')), ref_map2)
   | _ as x  -> (fncs, [], (t, x), ref_map)
   in
 let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
   let fvs'' = List.filter check_scope fvs' in
   (fncs', fvs'', expr', ref_map')
 and dfs_exprs fncs env exprs ref_map = match exprs with
      expr :: rest ->
              let (fncs1, fvs1, expr', ref_map1) = dfs_expr fncs env expr ref_map in
              let add_bind m (t, id) = StringMap.add id t m in
              let new_env = { variables = List.fold_left add_bind env.variables
              fvs1;
                              parent = env.parent;
              } in
              let (fncs2, fvs2, rest', ref_map2) = dfs_exprs fncs1 new_env rest ref_map1 in
              (fncs2, List.concat [fvs1; fvs2], expr' :: rest', ref_map2)
    | [] -> (fncs, [], exprs, ref_map)
 and build_closure  ?fname fncs env fexpr ref_map =
         f_id := !f_id + 1;
         let add_bind m (t, id) = StringMap.add id t m in
         let vars = List.fold_left add_bind StringMap.empty (to_formals fexpr.sformals) in
         let name = match fname with Some x -> x
                                   | None -> ""
         in
         let func_t = {
            typ_t = fexpr.srtyp;
            formals_t = List.map fst (to_formals fexpr.sformals);
         }
         in
         let t = SFunction(func_t)
         in
         let vars_rec = match name with "" -> vars
                                      | _  -> StringMap.add name t vars
         in
         let new_ref_map = StringMap.empty
         in
         let new_env = { variables = vars_rec;
                         parent = Some env } in
         let (fncs', fvs, _, body', ref_map') = dfs_stmts fncs new_env fexpr.sbody new_ref_map in
         let clsr = {
            ind = !f_id;
            fvs = List.map (find_and_replace ref_map') fvs;
         } 
         in
         let new_fnc = {
            lname = name;
            lfvs = List.map (find_and_replace ref_map') fvs;
            ltyp = fexpr.srtyp;
            (* Type inference from direct ref_map context for sfunction*)
            lformals =  List.map (find_and_replace ref_map') (to_formals fexpr.sformals);
            lbody = body';
         }
         in
         (new_fnc :: fncs', fvs, (SFunction(func_t), clsr), StringMap.fold StringMap.add ref_map ref_map')
(*Need to check if the gloabls and functions have correct type to avoid type casting*)
 let lift (globals, functions) =
         let is_global = function    
           SVarDef(var) -> var
         | SFuncDef(f) -> raise (Failure ("Global should not be function def"))
         in 
         let is_func = function    
          SVarDef(var) -> raise (Failure ("Function should not be function def"))
        | SFuncDef(f) -> f
        in
        let checked_global = List.map is_global globals
        in 
        let checked_func = List.map is_func functions
          in
         let add_symbol m def = 
          match def with 
            SVarDef(var) -> 
            (match var with
             SDecl (typ, name) -> StringMap.add name typ m
           | SInit(typ, sassign) ->  StringMap.add (fst sassign) typ m)
          | SFuncDef(func) -> let f = concrete_func func.srtyp func.sformals 
          in StringMap.add func.sfname (SFunction f) m
         in
         (* Add all non-main function definition to symbol table*)
          let symbols = List.fold_left add_symbol
           (StringMap.empty) (List.filter (fun def -> match def with 
           SVarDef(var) -> true
          | SFuncDef(f) -> f.sfname <> "main"
           ) functions@globals) 
         in
         let build_func f = 
          let symbols' = List.fold_left (fun m (typ, name) -> StringMap.add name typ m) 
            symbols (to_formals f.sformals)
            in
            let (fncs, _, _, stmts', ref_map) = dfs_stmts [] { variables = symbols';
            parent = None } f.sbody StringMap.empty
            in
              let f' = {
                lname = f.sfname;
                lfvs = [] ;
                ltyp = f.srtyp;
                (* Type inference from direct ref_map context for sfunction*)
                lformals = List.map (find_and_replace ref_map) (to_formals f.sformals);
                lbody = stmts';
              } in  
              let named_fncs = List.map (fun f -> (idx := !idx + 1; "f" ^ string_of_int !idx, f))
              (List.rev fncs)
            in
          ((f'.lname, f') :: named_fncs)
         in
         (checked_global, List.flatten (List.map build_func checked_func))



 (* Pretty-printing function *)
 let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SFloatLit(l) -> string_of_float l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SArrayLit(l) -> "{" ^ (String.concat ", " (List.map string_of_sexpr l)) ^ "}"
      | SId(s) -> s
      | SArrayAccess(var, idx) -> var ^ "[" ^ (string_of_int idx) ^ "]"
      | SStringLit(s) -> "\"" ^ s ^ "\""
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
      | SAssign(sassign) -> string_of_sassign sassign
      | SArrayAssign(v, i, e) -> v ^ "[" ^ (string_of_int i) ^ "]" ^ " = " ^ string_of_sexpr e
      | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SFuncExpr(expr) -> raise (Failure ("Function expression is not implemented in lambda"))
      | SClosure(clsr) -> "f" ^ (string_of_int (clsr.ind)) ^ "(" ^ String.concat ", " (List.map string_of_bind clsr.fvs) ^ ")"  
       ) ^ ")"
and string_of_sassign (string, sexpr) = string ^ " "^ string_of_sexpr sexpr
and string_of_bind (typ, string) = string_of_typ typ ^ " " ^  string
and string_of_svdecl = function
    SDecl (typ, string) -> string_of_typ typ ^ " " ^ string 
  | SInit (typ, sassign) -> string_of_typ typ ^ " " ^ string_of_sassign sassign 
and string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SLocalVarDef l -> string_of_svdecl l ^ ";\n"

and string_of_sfdecl (name, fdecl) =
  string_of_typ fdecl.ltyp ^ " " ^
  name ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.lformals) ^
  ") " ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.lfvs) ^ ") " ^ "{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.lbody) ^
  "}\n"

  let string_of_lprogram (defs) =
    "\n\nParsed program: \n\n" ^
    String.concat "" (List.map string_of_svdecl (fst defs)) ^ "\n"
    ^ String.concat "" (List.map string_of_sfdecl (snd defs)) ^ "\n"

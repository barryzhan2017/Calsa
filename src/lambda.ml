 open Sast
 open Ast
 
 module StringMap = Map.Make(String)
 
 
 (* Some types for lifting *)
 type environment = {
     variables : typ StringMap.t;
     parent: environment option;
 }
 (* Mix with sfunc_def*)
 type lfexpr = {
     name : string;
     fvs : bind list;
     styp : typ;
     sformals : bind list;
     sbody : sstmt list;
 }
 let built_in_decls =
     let empty_func t = ({ typ_t = t; sformals_t = [] }) in
     let add_default map (name, ty) = StringMap.add name (SFunction ty) map
     in List.fold_left add_default StringMap.empty [("print", empty_func Int); ("add", empty_func Int)]

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

 let rec dfs_stmt fncs env stmt =
   let (fncs', fvs', env', stmt') =
     match stmt with
     SBlock(stmts) ->
             let (fncs1, fvs1, _, stmts') = dfs_stmts fncs env stmts
             in (fncs1, fvs1, env, SBlock(stmts'))
   | SDcl(ty, id, e) ->
                   let (fncs1, fvs1, e1') = dfs_expr fncs env e ~fname:id in
                   let (t1, _) = e1' in
                   let new_typ = match (ty, t1) with
                                 SFunction(_), SFunction(_) -> t1
                               | _ -> ty
                   in
                   let new_env = {variables = StringMap.add id new_typ env.variables;
                   parent = env.parent} in
                   (fncs1, fvs1, new_env, SDcl(new_typ, id, e1'))
   | SExpr(e) ->
                   let (fncs1, fvs1, e1') = dfs_expr fncs env e in
                   (fncs1, fvs1, env, SExpr(e1'))
   | SReturn(e) ->
                   let (fncs1, fvs1, e1') = dfs_expr fncs env e in
                   (fncs1, fvs1, env, SReturn(e1'))
   | SIf(e, s1, s2) ->
              let (fncs1, fvs1, e') = dfs_expr fncs env e in
              let (fncs2, fvs2, _, s1') = dfs_stmt fncs1 env s1 in
              let (fncs3, fvs3, _, s2') = dfs_stmt fncs2 env s2 in
              (fncs3, List.concat [fvs1; fvs2; fvs3], env, SIf(e', s1', s2'))
   | SWhile(e, s) ->
              let (fncs1, fvs1, e') = dfs_expr fncs env e in
              let (fncs2, fvs2, _, s') = dfs_stmt fncs1 env s in
              (fncs2, List.concat [fvs1; fvs2], env, SWhile(e', s'))
   in
   let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
   let fvs'' = List.filter check_scope fvs' in
   (fncs', fvs'', env', stmt')
 and dfs_stmts fncs env stmts = match stmts with
      stmt :: the_rest ->
              let (fncs1, fvs1, env1, stmts1) = dfs_stmt fncs env stmt in
              let add_bind m (t, id) = StringMap.add id t m in
              let new_env = { variables = List.fold_left add_bind env1.variables
              fvs1;
                              parent = env1.parent;
              } in
              let (fncs2, fvs2, env2, stmts2) = dfs_stmts fncs1 new_env the_rest in
              (fncs2, List.concat [fvs1; fvs2], env2, stmts1 :: stmts2 )
    | [] -> (fncs, [], env, stmts)
 and dfs_expr ?fname fncs env e =
     let t = fst e in
     let expr = snd e in
     let  (fncs', fvs', expr') = match expr with
     SCall(s1, exprs) ->
              let fv' =
                      if (StringMap.mem s1 env.variables ||
                          StringMap.mem s1 built_in_decls)
                      then
                          None
                      else
                          Some(lookup env s1, s1)
              in
              let (fncs1, fvs1, exprs') = dfs_exprs fncs env exprs
              in let fvs' = match fv' with
                 Some(x) -> x :: fvs1
              |  _ -> fvs1
              in (fncs1, fvs', (t, SCall(s1, exprs')))
   | SId(s1) ->
              let fv =if (StringMap.mem s1 env.variables ||
                          StringMap.mem s1 built_in_decls)
                      then
                          []
                      else
                          [lookup env s1, s1]
              in
              (fncs, fv, (t, SId(s1)))
   | SFuncExpr(e1) ->
              let (fncs1, fvs1, (t, clsr)) = match fname with Some x -> build_closure
                                                              fncs env e1 ~fname:x
                                                            | None -> build_closure
                                                              fncs env e1
              in
              let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
              let fvs1' = List.filter check_scope fvs1 in
              (fncs1, fvs1', (t, SClosure(clsr)))
   | _ as x  -> (fncs, [], (t, x))
   in
 let check_scope (_, fv) = not (StringMap.mem fv env.variables) in
   let fvs'' = List.filter check_scope fvs' in
   (fncs', fvs'', expr')
 and dfs_exprs fncs env exprs = match exprs with
      expr :: rest ->
              let (fncs1, fvs1, expr') = dfs_expr fncs env expr in
              let add_bind m (t, id) = StringMap.add id t m in
              let new_env = { variables = List.fold_left add_bind env.variables
              fvs1;
                              parent = env.parent;
              } in
              let (fncs2, fvs2, rest') = dfs_exprs fncs1 new_env rest in
              (fncs2, List.concat [fvs1; fvs2], expr' :: rest')
    | [] -> (fncs, [], exprs)
 and build_closure  ?fname fncs env fexpr =
         let add_bind m (t, id) = StringMap.add id t m in
         let vars = List.fold_left add_bind StringMap.empty fexpr.sformals in
         let name = match fname with Some x -> x
                                   | None -> ""
         in
         let func_t = {
            typ_t = fexpr.srtyp;
            sformals_t = List.map fst fexpr.sformals;
         }
         in
         let vars_rec = match name with "" -> vars
                                      | _  -> StringMap.add name SFunction(func_t) vars
         in
         let new_env = { variables = vars_rec;
                         parent = Some env } in
         let (fncs', fvs, _, body') = dfs_stmts fncs new_env fexpr.sbody in
         let clsr = {
            ind = List.length fncs';
            free_vars = fvs;
         }
         in
         let new_fnc = {
            fname = name;
            fvs = fvs;
            styp = fexpr.srtyp;
            sformals = fexpr.sformals;
            sbody = body';
         }
        in
         (new_fnc :: fncs', fvs, (SFunction(func_t), clsr))
 let lift stmts =
         let (fncs, _, _, stmts') = dfs_stmts [] { variables = StringMap.empty ;
         parent = None } stmts in
         let main_fnc = {
                 name = "main";
                 fvs = [] ;
                 styp = Int;
                 sformals = [];
                 sbody = stmts';
         } in
         let named_fncs = List.mapi (fun i fnc -> ("f" ^ string_of_int i, fnc))
         (List.rev fncs)
         in (("main", main_fnc) ::  named_fncs)
 (* Pretty-printing function *)
 and string_of_lfexpr (name, fexpr) =
     "FUNCTION " ^ name ^ "-" ^ fexpr.name ^ " "
   ^ string_of_typ (typ_of_styp fexpr.styp) ^ " "
   ^ "( (fvs: " ^ String.concat ", " (List.map string_of_sformal fexpr.fvs) ^ "), ("
   ^ String.concat ", " (List.map string_of_sformal fexpr.sformals)
   ^ ") )\n{\n"
   ^ String.concat "" (List.map string_of_sstmt fexpr.sbody)
   ^ "}\n"
 let string_of_last fncs =
         "PROGRAM:\n"
       ^ String.concat "-----\n" (List.map string_of_lfexpr fncs)
 
 open Sast
 open Ast
 
 module StringMap = Map.Make(String)

let extract_svar  = function
  SDecl (typ, s) -> (typ, s)
| SInit(typ, sassign) -> (typ, fst sassign)

let to_formals fakes =  List.map extract_svar fakes

 (* Some types for lifting *)
 type environment = {
     variables : typ StringMap.t;
     parent: environment option;
}

type lfexpr = {
     name : string;
     free_vars : bind list;
     styp : typ;
     sformals : bind list;
     sbody : sstmt list;
 }

 let empty_func t = ({ typ_t = t; formals_t = [] }) 
 let concrete_func t f = ({ typ_t = t; formals_t = List.map fst (to_formals f) }) 

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

 let rec dfs_stmt fncs env stmt =
   let (fncs', fvs', env', stmt') =
     match stmt with
     SBlock(stmts) ->
             let (fncs1, fvs1, _, stmts') = dfs_stmts fncs env stmts
             in (fncs1, fvs1, env, SBlock(stmts'))
   | SLocalVarDef(def) -> (
                    match def with   
                    SInit(t, sass) -> 
                    let (fncs1, fvs1, e1') = dfs_expr fncs env (snd sass) ~fname:(fst sass) in
                      let (t1, _) = e1' in
                      let new_typ = match (t, t1) with
                                    SFunction(_), SFunction(_) -> t1
                                  | _ -> t
                      in
                      let new_env = {variables = StringMap.add (fst sass) new_typ env.variables;
                      parent = env.parent} in
                      let init = SInit(new_typ, ((fst sass), e1')) 
                      in
                      (fncs1, fvs1, new_env, SLocalVarDef(init))
                    (*Should we need this??*)
                  | SDecl (t, s) -> 
                      let new_env = {variables = StringMap.add (s) t env.variables;
                      parent = env.parent} 
                      in (fncs, [], new_env, SLocalVarDef(def)))
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
   | SAssign(s1, e1) ->
              let (fncs1, fvs1, e1') = dfs_expr fncs env e1 in
              (fncs1, fvs1, (t, SAssign(s1, e1')))
   | SArrayAssign(s1, idx, e1) -> 
              let (fncs1, fvs1, e1') = dfs_expr fncs env e1 in
              (fncs1, fvs1, (t, SArrayAssign(s1, idx, e1')))
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
         let new_env = { variables = vars_rec;
                         parent = Some env } in
         let (fncs', fvs, _, body') = dfs_stmts fncs new_env fexpr.sbody in
         let clsr = {
            ind = List.length fncs';
            fvs = fvs;
         }
         in
         let new_fnc = {
            name = name;
            free_vars = fvs;
            styp = fexpr.srtyp;
            sformals =  (to_formals fexpr.sformals);
            sbody = body';
         }
        in
         (new_fnc :: fncs', fvs, (SFunction(func_t), clsr))
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
         (* add all non-main function definition to symbol table*)
          let symbols = List.fold_left add_symbol
           (StringMap.empty) (List.filter (fun def -> match def with 
           SVarDef(var) -> true
          | SFuncDef(f) -> f.sfname <> "main"
           ) globals@functions) 
         in
         let main_func = List.find (fun f -> f.sfname = "main") checked_func
         in
         let (fncs, _, _, stmts') = dfs_stmts [] { variables = symbols ;
         parent = None } main_func.sbody in
         let main_fnc = {
                 name = "main";
                 free_vars = [] ;
                 styp = Int;
                 sformals = [];
                 sbody = stmts';
         } in
         let named_fncs = List.mapi (fun i fnc -> ("f" ^ string_of_int i, fnc))
         (List.rev fncs)
         in (checked_global, ("main", main_fnc) ::  named_fncs)


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
    SDecl (typ, string) -> string_of_typ typ ^ " " ^ string ^ "\n"
  | SInit (typ, sassign) -> string_of_typ typ ^ " " ^ string_of_sassign sassign ^ "\n"
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
  string_of_typ fdecl.styp ^ " " ^
  name ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.sformals) ^
  ")\n" ^ "(" ^ String.concat ", " (List.map string_of_bind fdecl.free_vars) ^ ")" ^ "{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_lprogram (defs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_svdecl (fst defs)) ^ "\n"
  ^ String.concat "" (List.map string_of_sfdecl (snd defs)) ^ "\n"




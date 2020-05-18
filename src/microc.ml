(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)
   open Sys
   open Printf
   
   type action = Ast | Sast | LLVM_IR | Exec
   
   let () =
     let lines = ref [] in
     let libchannel = open_in "./test/lib.mc" in
     try
        while true; do
          lines := input_line libchannel :: !lines
        done;
     with End_of_file -> 
        close_in libchannel;
     
     let prochannel = open_in Sys.argv.((Array.length Sys.argv) - 1) in
     try
        while true; do
          lines := input_line prochannel :: !lines
        done;
     with End_of_file -> 
        close_in prochannel;
     
     let outchannel = open_out "./test/output.a" 
     and cnt = List.rev !lines in
     List.iter (fun s -> fprintf outchannel "%s\n" s) cnt;
     close_out outchannel;

     let action = ref Exec in
     let set_action a () = action := a in
     let speclist = [
       ("-a", Arg.Unit (set_action Ast), "Print the AST");
       ("-s", Arg.Unit (set_action Sast), "Print the SAST");
       ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
     ] in
     let usage_msg = "usage: ./microc.native [-a|-s|-l] [file.mc]" in
     let channel = ref stdin in
     Arg.parse speclist (fun filename -> channel := open_in "./test/output.a") usage_msg;
   
     let lexbuf = Lexing.from_channel !channel in
   
     let ast = Microcparse.program Scanner.token lexbuf in
     match !action with
       Ast -> print_string (Ast.string_of_program ast)
     | _ -> let sast = Semant.check ast in
     let last = Lambda.lift sast in
     let llvm_module = Llvm.string_of_llmodule (Irgen.translate last) in
       match !action with
         Ast     -> ()
       | Sast    -> print_string (Sast.string_of_sprogram sast)
       | LLVM_IR -> print_string llvm_module
       | Exec -> let out = open_out "llvm.out" in
       fprintf out "%s\n" llvm_module; close_out out;
       if (command "llc -relocation-model=pic llvm.out" != 0)
       then raise (Failure "llc: non-zero exit code")
       else if 
         ((command "gcc llvm.out.s -L./ -llist -o a.out" )!= 0)
       then raise (Failure "gcc: non-zero exit code")
       else ()
   
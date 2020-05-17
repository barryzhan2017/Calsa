open Irgen

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Microcparse.program Scanner.token lexbuf in
  let sprogram = Semant.check program in
  let lprogram = Lambda.lift sprogram in
  Llvm.string_of_llmodule (Irgen.translate lprogram)

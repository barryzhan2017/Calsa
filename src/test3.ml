open Lambda

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Microcparse.program Scanner.token lexbuf in
  let sprogram = Semant.check program in
  let lprogram = Lambda.lift sprogram in
  print_endline (string_of_lprogram lprogram)

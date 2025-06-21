open MiniJava
open Format

let read_eval_print lexbuf file_name =
  flush stderr;
  flush stdout;
  begin 
    try
      let cs = Parser.toplevel Lexer.main lexbuf in
      let _ = Typing.typing cs in
      fprintf std_formatter "type checked\n";
      let _ = Eval.eval cs file_name in
      ()
    with
      | Parser.Error -> 
        let token = Lexing.lexeme lexbuf in
        fprintf err_formatter "Parser.Error: unexpected token %s\n" token;
        Lexing.flush_input lexbuf;
        lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_bol = 0}
      | Typing.Type_error msg ->
        fprintf err_formatter "Type error: %s\n" msg
      | Eval.Eval_error msg ->
        fprintf err_formatter "Runtime error: %s\n" msg
  end

let () =
  let file_name = ref None in
  let spec = Arg.align [] in
  let usage = "Usage: minijava <file.mj>" in
  let parse_argv arg = match !file_name with
    | None -> file_name := Some arg
    | Some _ -> raise @@ Arg.Bad "error: only one file can be specified"
  in Arg.parse spec parse_argv usage;
  match !file_name with
    | None -> 
      fprintf err_formatter "%s\n%s\n"
        "Error: No source file specified."
        usage;
      exit 1
    | Some file ->
      try
        let channel = open_in file in
        let lexbuf = Lexing.from_channel channel in
        lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file};
        read_eval_print lexbuf (String.capitalize_ascii @@ Filename.chop_extension file);
        close_in channel
      with Sys_error msg -> fprintf err_formatter "Error: %s\n" msg

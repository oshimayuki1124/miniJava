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
  end

let () =
  (* let program = Sys.argv.(0) in
  let file = ref None in
  let options = Arg.align [] in
  let parse_argv arg = match !file with
    | None -> file := Some arg
    | Some _ -> raise @@ Arg.Bad "error: only one file can be specified"
  in let usage = "" in
  Arg.parse options parse_argv usage; *)
  let file = Sys.argv.(1) in
  let channel = open_in file in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file};
  read_eval_print lexbuf (String.capitalize_ascii @@ Filename.chop_extension file);
  close_in channel

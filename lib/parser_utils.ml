(* Simple wrapper tying together parser and lexer *)

open Printf

module type Parser_type =
sig
  type parser_token
  type result
  val input : (Lexing.lexbuf -> parser_token) -> Lexing.lexbuf -> result
  val lexer_token : Lexing.lexbuf -> parser_token
  val rule_tail : string -> Lexing.lexbuf -> string
end

exception Error of exn * (int * int * int * int * string * string)

module Make(T : Parser_type) = struct
  
  let parse_buf_exn lexbuf =
    try
      T.input
        T.lexer_token
        lexbuf
    with exn ->
      begin
        let current_position = lexbuf.Lexing.lex_curr_p in
        let line = current_position.Lexing.pos_lnum in
        let pos_cnum = current_position.Lexing.pos_cnum in
        let cnum = current_position.Lexing.pos_cnum - current_position.Lexing.pos_bol in
        let pos_bol = current_position.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        let tail = T.rule_tail "" lexbuf in
        raise (Error (exn, (line, cnum, pos_cnum, pos_bol, tok, tail)))
      end
  
  let parse_buf filename lexbuf =
    try
      parse_buf_exn
        lexbuf
    with exn ->
      (
        match exn with
        | Error (exn, (line, cnum, pos_cnum, pos_bol, tok, tail)) ->
          print_endline
            (
              sprintf
                "Parser_utils: parse_buf: exception raised: in filename \"%s\": exn %s ; backtrace:\n%s\nline %d cnum %d pos_cnum %d pos_bol %d; tok \"%s\" tail:\n'%s'"
                filename
                (Printexc.to_string exn)
                (Printexc.get_backtrace ())
                line
                cnum
                pos_cnum
                pos_bol
                tok
                tail
            );
          exit 1
        | exn_ ->
          print_endline (sprintf "Parser_utils: parse_buf: in filename \"%s\": exception raised: unknown exception" filename);
          exit 1
      )
  
  let get_lexbuf_file filename =
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
    lexbuf
  
  let parse
      filepath
    =
    if Sys.file_exists filepath == false then
      (
        print_endline (sprintf "Parser_utils_functor: parse: cannot find file: \"%s\"" filepath);
        assert(false)
        (* exit 1 *)
      );

    if Sys.is_directory filepath then
      (
        print_endline (sprintf "Parser_utils_functor: parse: path is a directory: \"%s\"" filepath);
        assert(false)
        (* exit 1 *)
      );

    let lexbuf =
      get_lexbuf_file
        filepath
    in

    let rule_list =
      parse_buf
        filepath
        lexbuf
    in

    rule_list

end

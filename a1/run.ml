let string_of_unop u = match u with
  | Syntax.OpNeg -> "OpNeg"

let string_of_binop b = match b with
  | Syntax.OpPlus -> "OpPlus"
  | Syntax.OpMinus -> "OpMinus"
  | Syntax.OpTimes -> "OpTimes"

let rec string_of_expr e = match e with
  | Syntax.ENumLit n -> "ENumLit "^(string_of_int n)
  | Syntax.EUnOp (unop, e) ->
    String.concat
      ""
      [
        "EUnOp";
        " (";
        string_of_unop unop;
        ", ";
        string_of_expr e;
        ")"
      ]
  | Syntax.EBinOp (e1, binop, e2) ->
    String.concat
      ""
      [
        "EBinOp";
        " (";
        string_of_expr e1;
        ", ";
        string_of_binop binop;
        ", ";
        string_of_expr e2;
        ")";
      ]

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    (* Run the parser on this line of input. *)
    Printf.printf "\n%s\n%!" (string_of_expr (Parser.main Lexer.token linebuf))
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s\n%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "\nAt offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel

let () =
  repeat (Lexing.from_channel stdin)

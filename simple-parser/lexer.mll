{
  open Parser

  exception Error of string

}

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)

rule line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

and token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| ['0'-'9']+ as i
    { print_string "INT(";
      print_string i;
      print_string ") ";
      flush stdout;

      INT (int_of_string i) }
| '+'
    { print_string "PLUS ";
      flush stdout;
        
      PLUS }
| '-'
    { print_string "MINUS ";
      flush stdout;
    
      MINUS }
| '*'
    { print_string "TIMES ";
      flush stdout;
     
      TIMES }
| '('
    { print_string "LPAREN ";
      flush stdout;
    
      LPAREN }
| ')'
    { print_string "RPAREN ";
      flush stdout;
    
      RPAREN }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }


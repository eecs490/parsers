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
| ".0"
    { LPRJ }
| ".1"
    { RPRJ }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '<'
    { LT }
| '>'
    { GT }
| "=?"
    { EQ }
| "Unit"
    { UNIT }
| "fix"
    { FIX }
| ','
    { COMMA }
| "Rec"
    { REC }
| "Forall"
    { FORALLT }
| "roll"
    { ROLL }
| "unroll"
    { UNROLL }
| "forall"
    { FORALL }
| '['
    { LSQUARE }
| ']'
    { RSQUARE }
| '@'
    { AT }
| '.'
    { DOT }
| 'L'
    { L }
| 'R'
    { R }
| "case"
    { CASE }
| "of"
    { OF }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| ':'
    { OFTYPE }
| "be"
    { BE }
| "in"
    { IN }
| "->"
    { ARROW }
| "fun"
    { FUN }
| "True"
    { BOOLLIT (true) }
| "False"
    { BOOLLIT (false) }
| "Num"
    { NUM }
| "Bool"
    { BOOL }
| ['\'']['a'-'z''_']['a'-'z''A'-'Z''\'''0'-'9''_']* as i
    { TID (i) }
| ['a'-'z''_']['a'-'z''A'-'Z''\'''0'-'9''_']* as i
    { EID (i) }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }


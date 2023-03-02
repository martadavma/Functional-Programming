{
  open Lexing
  open Impparser
  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1 }
}

let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+
let int = ['0'-'9']+
let word = ['a'-'z' 'A'-'Z']+

rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int     { Int (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'     { Plus }
  | '-'     { Minus }
  | "true"  { Bool (true) }
  | "false" { Bool (false) }
  | "="     { Asgn }
  | "!"     { Not }
  | "<"     { Lt }
  | "<="    { Leq }
  | "=="    { Eq }
  | "&&"    { And }
  | "||"    { Or }
  | "("     { LParen }
  | ")"     { RParen }
  | "skip"  { Skip }
  | ";"     { Semicolon }
  | "if"    { If }
  | "then"  { Then }
  | "else"  { Else }
  | "fi"    { Fi }
  | "while" { While }
  | "do"    { Do }
  | "done"  { Done }
  | word    { Var (Lexing.lexeme lexbuf) }
  | _       { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof     { EOF }

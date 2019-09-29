{
  module L = Lexing

  type token = [%import: Parser.token] [@@deriving show]

  let illegal_character loc char =
    Error.error loc "illegal character '%c'" char

  let unterminated_comment loc =
    Error.error loc "unterminated comment"

  let unterminated_string loc =
    Error.error loc "unterminated string"

  let illegal_escape loc sequence =
    Error.error loc "illegal escape sequence '%s' in string literal" sequence

  let set_filename lexbuf fname =
    lexbuf.L.lex_curr_p <-  
      { lexbuf.L.lex_curr_p with L.pos_fname = fname }

  (* a string buffer to accumulate characters when scanning string literals *)
  let string_buffer = Buffer.create 16

  (* helper function to update new line counting while scanning string literals *)
  let str_incr_linenum str lexbuf =
    String.iter (function '\n' -> L.new_line lexbuf | _ -> ()) str
}

let spaces = [' ' '\t'] +
let digit = ['0'-'9']
let litint = digit +
let char = ['a'-'z' 'A'-'Z']
let id = char + (char | '_' | digit)*
let real = ((digit*('.')digit+ | digit+('.')digit*)(('e'|'E')('-')?(digit+))?) 
           | (digit+ ('e'|'E') ('-')? digit+)
let bool = ("true"|"false")

rule token = parse
  | spaces               { token lexbuf }
  | '\n'                 { L.new_line lexbuf;
                           token lexbuf }
  | litint as lxm        { INTEGER (int_of_string lxm) }
  | real as lxm          { REAL (float_of_string lxm) }
  | bool as lxm          { LOGIC (bool_of_string lxm) }
  | '"'                  { string lexbuf.L.lex_start_p lexbuf }
  | "if"                 { IF }
  | "then"               { THEN }
  | "else"               { ELSE }
  | "while"              { WHILE }
  | "do"                 { DO }
  | "break"              { BREAK }
  | "let"                { LET }
  | "in"                 { IN }
  | "end"                { END }
  | "var"                { VAR }
  | id as lxm            { ID (Symbol.symbol lxm) }
  | '('                  { LPAREN }
  | ')'                  { RPAREN }
  | ':'                  { COLON }
  | ','                  { COMMA }
  | ';'                  { SEMI }
  | '+'                  { PLUS }
  | '-'                  { MINUS }
  | '*'                  { TIMES }
  | '/'                  { DIV }
  | '%'                  { MOD }
  | '^'                  { POW }
  | '='                  { EQ }
  | "<>"                 { NE }
  | '<'                  { LT }
  | "<="                 { LE }
  | '>'                  { GT }
  | ">="                 { GE }
  | '&'                  { AND }
  | '|'                  { OR }
  | ":="                 { ASSIGN }
  | eof                  { EOF }
  | _                    { illegal_character (Location.curr_loc lexbuf) (L.lexeme_char lexbuf 0) }

and string pos = parse
| '"'                                                  { lexbuf.L.lex_start_p <- pos;
                                                         let text = Buffer.contents string_buffer in
                                                         Buffer.clear string_buffer;
                                                         STRING text }
| "\\t"                                                { Buffer.add_char string_buffer '\t';
                                                         string pos lexbuf }
| "\\n"                                                { Buffer.add_char string_buffer '\n';
                                                         string pos lexbuf }               
| "\\r"                                                { Buffer.add_char string_buffer '\r';
                                                         string pos lexbuf }
| "\\b"                                                { Buffer.add_char string_buffer '\b';
                                                         string pos lexbuf }
| "\\v"                                                { Buffer.add_char string_buffer (Char.chr 11);
                                                         string pos lexbuf }
| "\\f"                                                { Buffer.add_char string_buffer (Char.chr 12);
                                                         string pos lexbuf }
| "\\^" (['A'-'Z' '@' '[' '\\' ']' '^' '_' '?'] as x)  { Buffer.add_char string_buffer x;
                                                         string pos lexbuf }
| "\\" (digit digit digit as x)                        { Buffer.add_string string_buffer x; 
                                                         string pos lexbuf }
| [^ '\\' '"']+ as lxm                                 { str_incr_linenum lxm lexbuf;
                                                         Buffer.add_string string_buffer lxm;
                                                         string pos lexbuf }
| eof                                                  { unterminated_string (pos, lexbuf.L.lex_start_p); }



{ open Parser }

let Decimal = '.' ['0'-'9']+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     		{ comment lexbuf }      (* Comments *)
| '['			{ LBRACK }
| ']'			{ RBRACK }
| '('			{ LPAREN }
| ')'			{ RPAREN }
| '{'			{ LBRACE }
| '}'			{ RBRACE }
| ';'			{ SEMI }
| ','      		{ COMMA }
| '^'			{ VIB }
| '~'			{ TREM }
| '+'			{ PLUS }
| "++"			{ INCR }
| '-'			{ MINUS }
| "--"			{ DECR }
| '*'			{ TIMES }
| '/'			{ DIVIDE }
| '.'			{ SERIAL }
| ':'			{ PARALLEL }
| "->"			{ ARROW }
| '='			{ ASSIGN }
| "=="			{ EQ }
| "!="			{ NEQ }
| '<'			{ LT }
| "<="			{ LEQ }
| '>'			{ GT }
| ">="			{ GEQ }
| "if"			{ IF }
| "else"		{ ELSE }
| "for"			{ FOR }
| "while"		{ WHILE }
| "loop"		{ LOOP }
| "return"		{ RETURN }
| "fun"			{ FUN }
| "vol"			{ VOL }
| "dur"			{ DUR }
| "pitch"		{ PITCH }
| "double"		{ DOUBLE }
| "note"		{ NOTE }
| "rest"		{ REST }
| "track"		{ TRACK }
| "chord"		{ CHORD }
| "score"		{ SCORE }
| "print"		{ PRINT }
(*
| "array"		{ ARRAY } 
*)
| '-'? Decimal as lxm { LITERAL(lxm) } (* Note in dj literals are really only doubles *)
| '-'? ['0'-'9']+ Decimal? as lxm { LITERAL(lxm) } (* Note in dj literals are really only doubles *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }



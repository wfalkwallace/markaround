{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     		{ comment lexbuf }      (* Comments *)
| '['			{ LBRACK }
| ']'			{ RBRACK }
| '('			{ LPAREN }
| ')'			{ RPAREN }
| '{'			{ LBRACE }
| '}'			{ RBRACE }
| '!'			{ EXP }
| '^'			{ VIB }
| '~'			{ TREM }
| '*'			{ STAR }
| '\'			{ FSLASH }
| ':'			{ COLON }
| "->"			{ RARROW }
| "<-"			{ LARROW }
| "=="			{ DEQ }
| "!="			{ NEQ }
| '<'			{ LT }
| '>'			{ GT }
| "include"		{ INCL }

| '-'? Decimal as lxm { LITERAL(lxm) } (* Note in dj literals are really only doubles *)
| '-'? ['0'-'9']+ Decimal? as lxm { LITERAL(lxm) } (* Note in dj literals are really only doubles *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }



{ open Parser }

rule token = parse
  ' '			{ token lexbuf } 			(* Whitespace *)
| '\t'			{ TAB }
| '\r'			{ NEW }
| '\n'			{ NEW }
| "/*"     		{ comment lexbuf }      	(* Comments *)
| "//"     		{ commentline lexbuf }      (* Comments *)
| '['			{ LBRACK }
| ']'			{ RBRACK }
| '('			{ LPAREN }
| ')'			{ RPAREN }
| '{'			{ LBRACE }
| '}'			{ RBRACE }
| '!'			{ EXP }
| '^'			{ HAT }
| '~'			{ TILDE }
| '*'			{ STAR }
| '\'			{ BACKSLASH }
| '/'			{ FRONTSLASH }
| '&'			{ AMP }
| '`'			{ TICK }
| '''			{ SQUOTE }
| '"'			{ DQUOTE }
| '|'			{ BAR }
| '::'			{ COLONS }
| "-"			{ DASH }
| "->"			{ RARROW }
| "-->"			{ RARROWS }
| "<-"			{ LARROW }
| "<--"			{ LARROWS }
| "=="			{ DEQ }
| "!="			{ NEQ }
| '<'			{ LT }
| '>'			{ GT }
| "include"		{ INCL }

| '-'? ['0'-'9']+ Decimal? as lxm { LITERAL(lxm) } (* are numbers treated differently? what about escape sequences? *)
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("Illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and commentline = parse
  ['\r' '\n'] { token lexbuf }
| _    { comment lexbuf }

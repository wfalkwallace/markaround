%{ open Ast %}

%token TAB NEW BACKSLASH FRONTSLASH
%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE
%token EXP HAT TILDE STAR AMP
%token TICK SQUOTE DQUOTE
%token BAR COLONS DASH RARROW RARROWS LARROW LARROWS 
%token DEQ NEQ LT GT 
%token INCL

/*
%token <string> LITERAL
%token <string> ID
%token EOF

%left EQ NEQ
%left LT GT LEQ GEQ
%left SERIAL PARALLEL
%left PLUS MINUS
%left TIMES DIVIDE
%left VIB TREM
*/


%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }




formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    formal                   { [$1] }
  | formal_list COMMA formal { $3 :: $1 }

vdecl:
   dType ID     { { vType = $1;  vName = $2; } }

vinit:
    vdecl ASSIGN expr { Vinit($1, $3) }

assign:
    expr ASSIGN expr { Assign($1, $3) }

/* --- SCORE -- */
score_cr:
    SCORE LPAREN RPAREN { SCORE_CR ([]) }
    | SCORE LPAREN score_list RPAREN { SCORE_CR ( List.rev $3 ) }

score_list:
    expr { [$1] }
    | score_list COMMA expr { $3 :: $1 }

/* --- TRACK -- */
track_cr:
  TRACK LPAREN expr RPAREN { TRACK_CR( $3 ) }

/* --- REST --- */
rest_cr:
  REST LPAREN expr RPAREN { REST_CR( $3 ) }

/*  --- NOTE  --- */
note_cr:
  NOTE LPAREN expr COMMA expr COMMA expr RPAREN { NOTE_CR($3, $5, $7) }

/* --- CHORD --- */
chord_cr:
    CHORD LPAREN RPAREN { CHORD_CR ([]) }
    | CHORD LPAREN chord_list RPAREN { CHORD_CR ( List.rev $3 ) }

chord_list:
    expr { [$1] }
    | chord_list COMMA expr { $3 :: $1 }

/* --- ACCESSOR --- */

accessor:
  ID ARROW note_attribute { ACCESSOR(Id($1), $3) }

/*
accessor:
  data_type_acc { $1 }

data_type_acc: 
  note_cr ARROW note_attribute { ACCESSOR($1, $3) }
*/

/* List of note attributes */
note_attribute:
  PITCH {Pitch}
  | VOL {Vol}
  | DUR {Dur}
  
dType: 
   DOUBLE {Double}
 | NOTE {Note}
 | CHORD {Chord} 
 | TRACK {Track}
 | REST {Rest}
 | SCORE {Score}


stmt:
    expr SEMI { Expr($1) }
  | vinit SEMI { $1 }
  | vdecl SEMI { Vdecl($1) }
  | RETURN expr SEMI { Return($2) }
  | PRINT LPAREN expr RPAREN SEMI { Print($3) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
  | LOOP LPAREN expr RPAREN stmt { Loop($3, $5) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

/* --- EXPRESSIONS --- */

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | ID               { Id($1) }
  | assign           { $1 }
  | accessor         { $1 }
  | chord_cr         { $1 }
  | note_cr          { $1 }
  | rest_cr          { $1 }
  | track_cr         { $1 }
  | score_cr         { $1 }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr SERIAL expr { Binop($1, Ser, $3) }
  | expr PARALLEL expr { Binop ($1, Par, $3) }
  | expr INCR        { Modifier($1, Incr) }
  | expr DECR        { Modifier($1, Decr) }
  | expr VIB         { Modifier($1, Vib) }
  | expr TREM        { Modifier($1, Trem) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | ID LBRACK expr RBRACK { Address(Id($1), $3) }
  /*| LBRACKET actuals_opt RBRACKET { Array($?) } */

 /* actuals - When you call the function you use actuals_opt?? */
  actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

  actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }


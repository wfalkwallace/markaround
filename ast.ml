(* AST *)
type modif = Vib | Trem | Incr | Decr

(* Not sure if I should make this a string *)
type note_attribute = Pitch | Vol | Dur

(*our data types*)
type dType = Double | Note | Chord | Track | Rest | Score

(* operation types *)
type op =   Add  | Sub
          | Mult | Div 
          | Ser  | Par  
          | Equal | Neq | Geq | Leq | Greater | Less

(* Expression type *)
type expr =
    Literal of string
  | Id of string
  | NOTE_CR of expr * expr * expr
  | REST_CR of expr
  | TRACK_CR of expr
  | CHORD_CR of expr list
  | SCORE_CR of expr list
  | ACCESSOR of expr * note_attribute
  | Binop of expr * op * expr
  | Modifier of expr * modif 
  | Assign of expr * expr
  | Address of expr * expr
  | Call of string * expr list
  | Noexpr
 
  (* | Array of expr list *)
  (*an array can be a list of expressions*)

(*variable declaration*)
type var_decl = {
  vType : dType;
  vName : string;
}

type var_init = {
  vDecl : var_decl;
  vExpr : expr;
}

(*need to decide if we are keeping loop or not*)
type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | Print of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | Loop of expr * stmt
  | While of expr * stmt
  (* | Assign of var_decl * expr *)
  | Vdecl of var_decl
  | Vinit of var_decl * expr  
 (* | Loop of expr * expr * stmt *)


(* funciton declaration *)
type func_decl = {
    rtype : dType;
    fname : string;
    formals : var_decl list;
    body : stmt list;
  }


(*ast is a list of variables and list of function dels*)
type program = var_decl list * func_decl list

(*pretty print for expr*)
(*TODO need to decide on arrays*)
let rec string_of_expr = function
    Literal(l) -> l
  | Id(s) -> s
  | NOTE_CR(a, b, c) ->
      "(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ", " ^ string_of_expr c ^ ")"
  | REST_CR(expr) -> "(" ^ string_of_expr expr ^ ")" (* should this really be string of literal or something? *)
  | TRACK_CR(expr) -> "(" ^ string_of_expr expr ^ ")"
  | SCORE_CR(expr_list) -> 
      "(" ^ String.concat " . " (List.map string_of_expr expr_list) ^ ")"
  | ACCESSOR(a, b) -> 
      (string_of_expr a) ^ " -> " ^ (
      match b with
        Pitch -> "pitch" | Vol -> "vol" | Dur -> "dur"
      )
  | Assign(id, expr) -> string_of_expr id ^ " = " ^ string_of_expr expr
  | Address(id, expr) -> string_of_expr id ^ "[" ^ string_of_expr expr ^ "]"
  | CHORD_CR(expr_list) -> 
      "(" ^ String.concat " : " (List.map string_of_expr expr_list) ^ ")"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	    Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">="
      | Ser -> "." | Par -> ":") ^ " " ^
      string_of_expr e2
  (*again, not sure about this section*)
  | Modifier(e1, modif) ->
      string_of_expr e1 ^
      (match modif with
      Vib -> "^" | Trem -> "~" | Incr -> "++" | Decr -> "--")
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
(*| Array*) 

let string_of_vdecl v = 
  (match v.vType with
    Double -> "double "
    | Note -> "note "
    | Chord -> "chord "
    | Track -> "track "
    | Rest -> "rest "
    | Score -> "score " ) ^ v.vName

(*
let string_of_cr_type t =
    (match )
*)
(*pretty print for stmts*)
(*TODO need to do loop*)
let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | Print(expr) -> "print (" ^ string_of_expr expr ^ ");\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | Loop(e, s) -> "loop (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  (* | Assign(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e *)
  | Vdecl(v) -> string_of_vdecl v ^ ";\n"
  | Vinit(v, e) -> string_of_vdecl v ^ " = " ^ string_of_expr e ^ ";\n"

 (*| Loop*)


let string_of_fdecl fdecl =
   (match fdecl.rtype with
    Double -> "double "
    | Note -> "note "
    | Chord -> "chord "
    | Track -> "track "
    | Rest -> "rest "
    | Score ->  "score ") ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

(*pretty print for program*)
let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)  


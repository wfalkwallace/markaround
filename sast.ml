(* SAST *)
type modif_t = Vib_t | Trem_t | Incr_t | Decr_t

(* Not sure if I should make this a string *)
type note_attribute_t = Pitch_t | Vol_t | Dur_t

type dType_t = Double_t | Note_t | Chord_t | Track_t | Rest_t | Score_t

(* operation types *)
type op_t =   Add_t  | Sub_t
          | Mult_t | Div_t 
          | Ser_t  | Par_t  
          | Equal_t | Neq_t | Geq_t | Leq_t | Greater_t | Less_t

(* Expression type *)
(* Expression type *)
(*
type expr_t =
    Literal of int
  | Id of string
  | NOTE_CR of string * string * string
  | REST_CR of string
  | CHORD_CR of string list
  | TRACK_CR of string
  | ACCESSOR of string * note_attribute_t
  | Binop of expr_t * op_t * expr_t
  | Modifier of expr_t * modif_t 
  | Assign of string * expr_t
  | Call of string * expr_t list
  | Noexpr
*)
type expr_t =
    Literal_t of string
  | Id_t of string
  | NOTE_CR_t of expr_t * expr_t * expr_t
  | REST_CR_t of expr_t
  | TRACK_CR_t of expr_t
  | CHORD_CR_t of expr_t list
  | SCORE_CR_t of expr_t list
  | ACCESSOR_t of expr_t * note_attribute_t
  | Binop_t of expr_t * op_t * expr_t
  | Modifier_t of expr_t * modif_t
  | Assign_t of expr_t * expr_t
  | Address_t of expr_t * expr_t
  | Call_t of string * expr_t list
  | Noexpr_t


  (* | Array of expr list *)
  (*an array can be a list of expressions*)

(*variable declaration*)
type var_decl_t = {
  vType_t : dType_t;
  vName_t : string;
}

type var_init_t = {
  vDecl_t : var_decl_t;
  vExpr_t : expr_t;
}

(*need to decide if we are keeping loop or not*)
type stmt_t =
    Block_t of stmt_t list
  | Expr_t of expr_t
  | Return_t of expr_t
  | Print_t of expr_t
  | If_t of expr_t * stmt_t * stmt_t
  | For_t of expr_t * expr_t * expr_t * stmt_t
  | Loop_t of expr_t * stmt_t
  | While_t of expr_t * stmt_t
  | Vdecl_t of var_decl_t
  | Vinit_t of var_decl_t * expr_t


(* funciton declaration *)
type func_decl_t = {
    rtype_t : dType_t;
    fname_t : string;
    formals_t : var_decl_t list;
    body_t : stmt_t list;
  }

(*ast is a list of variables and list of function dels*)
type program_t = var_decl_t list * func_decl_t list


let rec string_of_expr_t = function
    Literal_t(l) -> l
  | Id_t(s) -> s
  | NOTE_CR_t(a, b, c) ->
      "note (" ^ string_of_expr_t a ^ ", " ^ string_of_expr_t b ^ ", " ^ string_of_expr_t c ^ ")"
  | REST_CR_t(r) -> "rest (" ^ string_of_expr_t r ^ ")" 
  | TRACK_CR_t(track) -> "track (" ^ string_of_expr_t track ^ ")" 
  | SCORE_CR_t(score_list) -> 
  "score (" ^ String.concat " : " (List.map string_of_expr_t score_list) ^ ")"
  | ACCESSOR_t(a, b) -> 
      (string_of_expr_t a) ^ " -> " ^ (
      match b with
        Pitch_t -> "pitch" | Vol_t -> "vol" | Dur_t -> "dur"
      )
  | Assign_t(id, expr) -> string_of_expr_t id ^ " = " ^ string_of_expr_t expr
  | Address_t(id, expr) -> string_of_expr_t id ^ " [" ^ string_of_expr_t expr ^ "]"
  | CHORD_CR_t(note_list) -> 
      "chord (" ^ String.concat " : " (List.map string_of_expr_t note_list) ^ ")"
  | Binop_t(e1, o, e2) ->
      string_of_expr_t e1 ^ " " ^
      (match o with
      Add_t -> "+" | Sub_t -> "-" | Mult_t -> "*" | Div_t -> "/"
      | Equal_t -> "==" | Neq_t -> "!="
      | Less_t -> "<" | Leq_t -> "<=" | Greater_t -> ">" | Geq_t -> ">="
      | Ser_t -> "." | Par_t -> ":") ^ " " ^
      string_of_expr_t e2
  | Modifier_t(e1, modif) ->
      string_of_expr_t e1 ^
      (match modif with
      Vib_t -> "^" | Trem_t -> "~" | Incr_t -> "++" | Decr_t -> "--")
  | Call_t(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr_t el) ^ ")"
  | Noexpr_t -> ""


let string_of_vdecl_t v = 
  (match v.vType_t with
    Double_t -> "double "
    | Note_t -> "note "
    | Chord_t -> "chord "
    | Track_t -> "track "
    | Rest_t -> "rest "
    | Score_t -> "score ") ^ v.vName_t


let rec string_of_stmt_t = function
    Block_t(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt_t stmts) ^ "}\n"
  | Expr_t(expr) -> string_of_expr_t expr ^ ";\n";
  | Return_t(expr) -> "return " ^ string_of_expr_t expr ^ ";\n";
  | Print_t(expr) -> "print (" ^ string_of_expr_t expr ^ ");\n";
  | If_t(e, s, Block_t([])) -> "if (" ^ string_of_expr_t e ^ ")\n" ^ string_of_stmt_t s
  | If_t(e, s1, s2) ->  "if (" ^ string_of_expr_t e ^ ")\n" ^
      string_of_stmt_t s1 ^ "else\n" ^ string_of_stmt_t s2
  | For_t(e1, e2, e3, s) ->
      "for (" ^ string_of_expr_t e1  ^ " ; " ^ string_of_expr_t e2 ^ " ; " ^
      string_of_expr_t e3  ^ ") " ^ string_of_stmt_t s
  | Loop_t(e, s) -> "loop (" ^ string_of_expr_t e ^ ") " ^ string_of_stmt_t s
  | While_t(e, s) -> "while (" ^ string_of_expr_t e ^ ") " ^ string_of_stmt_t s
  | Vdecl_t(v) -> string_of_vdecl_t v ^ ";\n"
  | Vinit_t(v, e) -> string_of_vdecl_t v ^ " = " ^ string_of_expr_t e ^ ";\n"


let string_of_fdecl_t fdecl =
  fdecl.fname_t ^
   (match fdecl.rtype_t with
    Double_t -> " double "
    | Note_t -> " note "
    | Chord_t -> " chord "
    | Track_t -> " track "
    | Rest_t -> " rest "
    | Score_t -> " score ") ^ "(" ^ String.concat ", " (List.map string_of_vdecl_t fdecl.formals_t) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt_t fdecl.body_t) ^
  "}\n"

(*pretty print for program*)
let string_of_program_t (vars, funcs) =
  String.concat "" (List.map string_of_vdecl_t vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl_t funcs)  




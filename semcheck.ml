(* 

	semcheck.ml

	- semantically checks the input file
		- checks for type errors
		- undefined variable & function errors
	-converts from ast to sast

*)
open Sast
open Ast




(*NOTE:
	map.find: returns the value associated with a key
	map.mem: checks if value exists for a given key
*)


module StringMap = Map.Make(String)

type env = {
	functions:	string list StringMap.t;
	globals:	string StringMap.t;
	locals:		string StringMap.t;
}


(* TYPE CONVERSIONS *)

(* var type -> string *)
let string_of_vartype = function
   Ast.Double -> "double"
   | Ast.Note -> "note"
   | Ast.Rest -> "rest"
   | Ast.Chord -> "chord"
   | Ast.Track -> "track"
   | Ast.Score -> "score"

(* ast -> sast type*)
let ast_to_sast_note_attr = function
	Ast.Pitch -> Sast.Pitch_t
	| Ast.Vol -> Sast.Vol_t
	| Ast.Dur -> Sast.Dur_t
	(* | _ -> raise (Failure ("Mismatch Note Attribute Type")) *)
  
(* ast -> sast type*)
let ast_to_sast_op = function
	  Ast.Add -> Sast.Add_t
	| Ast.Sub -> Sast.Sub_t
	| Ast.Mult -> Sast.Mult_t
	| Ast.Div -> Sast.Div_t
	| Ast.Ser -> Sast.Ser_t
	| Ast.Par -> Sast.Par_t
	| Ast.Equal -> Sast.Equal_t
	| Ast.Neq -> Sast.Neq_t
	| Ast.Geq -> Sast.Geq_t
	| Ast.Leq -> Sast.Leq_t
	| Ast.Greater -> Sast.Greater_t
	| Ast.Less -> Sast.Less_t
	(* | _ -> raise (Failure ("Mismatch Operator Type")) *)
	
let ast_to_sast_mod = function
	  Ast.Vib -> Sast.Vib_t
	| Ast.Trem -> Sast.Trem_t
	| Ast.Incr -> Sast.Incr_t
	| Ast.Decr -> Sast.Decr_t
	(* | _ -> raise (Failure ("Mismatch Modifier Type")) *)

(* ast -> sast type*)
let ast_to_sast_type = function
   Ast.Double -> Sast.Double_t
   | Ast.Note -> Sast.Note_t
   | Ast.Rest -> Sast.Rest_t
   | Ast.Chord -> Sast.Chord_t
   | Ast.Track -> Sast.Track_t
   | Ast.Score -> Sast.Score_t
   (* | _ -> raise (Failure ("Mismatch Variable Type Type")) *)
   (* 
let ast_to_sast_vdecl vdecl = 
	let sast_type = (* ast_to_sast_type *) vdecl.vType in
		Sast.var_decl_t( {vType=sast_type; vName=vdecl.vName;} )
 *)

(* we may need a variable total conversion from
ast to sast *)

(*need for locals, formals, and global variabes*)
let convert_types vardecl = 
(* Sast.Vdecl_t( {vType_t= (ast_to_sast_type vardecl.vType); vName_t=vardecl.vName;} ) *)
( {vType_t= (ast_to_sast_type vardecl.vType); vName_t=vardecl.vName;} )

 (* TYPES - do we need this? *)
let get_type = function
	var -> string_of_vartype var.vType
	

(* HELPFUL FUNCTIONS TO GET AND ADD VARIABLES (GLOBAL & LOCAL), FUNCIONS TO ENVIRONMENT *)

(* 
	get_variable vname env
		vname - variable name
		env - environment stringmap
	Looks to find variable name in env's local list.
	If it doesn't find it, it checks the env's global list.
	If not found, raises error.
*)
let get_variable_name vname env = 
	try StringMap.find vname env.locals
	with Not_found -> try StringMap.find vname env.globals
						with Not_found -> raise (Failure ("Undeclared variable " ^ vname))

let get_variable_type vname env = 
	try StringMap.find vname env.locals
	with Not_found -> try StringMap.find vname env.globals
						with Not_found -> raise (Failure ("Untyped variable " ^ vname))


(* 
	get_function vname env
		vname - function name
		env - environment stringmap
	Looks to find function name in env's function list.
	If not found, raises error.
*)
let get_function fname env =
	try StringMap.find fname env.functions
	with Not_found -> raise (Failure ("Undeclared function " ^ fname))


(*
	add_local var_type name env
		var_type - variable type
		name - variable name
		env - environment stringmap
	Checks to see if the name is in env's local list.
	If it doesn't contain it, it adds it to the env's local list.
*)
let add_local var_type name env =
	if StringMap.mem name env.locals then raise (Failure ("Local variable " ^ name ^ "is already defined"))
	else StringMap.add name (string_of_vartype var_type) env.locals

(*
	add_global var_type name env
		var_type - variable type
		name - gariable name
		env - environment stringmap

	Checks to see if the add_localname is in the env's global list.
	If it dlesn't contain it, it adds it to the env's global list.
*)
let add_global var_type name env =
	(* if name exists in env.globals, return empty stringmap *)
	if StringMap.mem name env.globals then raise (Failure ("Global variable " ^ name ^ " is already defined."))
	(*  else; add to env.globals:
		key = name
		value = vartype 
	*)
	else StringMap.add name (string_of_vartype var_type) env.globals

(*
	CONFUSED ON THE GET_TYPE 
	add_function fname return formals env
		fname - function name
		rtype - return type
		formals - formal arguments
		env - environment stringmap

	Checks to see if the fname is in env's function list
	if not- it gets the types of the formals, adds:
		name, vartype of return, formals to environemt's function
*)
let add_function fname rtype formals env =
	if StringMap.mem fname env.functions then raise (Failure ("function " ^ fname ^ " is already defined."))
	else let fmls = List.map get_type formals in
	(* weird parenthesis...*)
	StringMap.add fname (string_of_vartype (rtype) :: fmls) env.functions
	(*Strinmap.add, parse locals, add to env*)


(* SEMANTIC CHECKING FUNCTIONS *)



let rec build_expr = function
	  Ast.Literal(i) -> Sast.Literal_t(i)
    | Ast.Id(i) -> Sast.Id_t(i)
	| Ast.ACCESSOR(expr, note_attr) -> Sast.ACCESSOR_t( (build_expr expr), (ast_to_sast_note_attr note_attr) )
	| Ast.NOTE_CR(expr1, expr2, expr3) -> Sast.NOTE_CR_t( (build_expr expr1), (build_expr expr2), (build_expr expr3) )
	| Ast.REST_CR(expr) -> Sast.REST_CR_t( (build_expr expr) )
	| Ast.CHORD_CR(expr_list) -> Sast.CHORD_CR_t( (build_expr_list expr_list) )
	| Ast.TRACK_CR(expr) -> Sast.TRACK_CR_t( (build_expr expr) )
	| Ast.SCORE_CR(expr_list) -> Sast.SCORE_CR_t( (build_expr_list expr_list) )
	| Ast.Binop(expr1, op, expr2) -> Sast.Binop_t( (build_expr expr1), (ast_to_sast_op op) , (build_expr expr2) )
	| Ast.Modifier(expr, m) -> Sast.Modifier_t( (build_expr expr), (ast_to_sast_mod m) )
	| Ast.Assign(expr1, expr2) -> Sast.Assign_t( (build_expr expr1), (build_expr expr2) ) 
	| Ast.Address(expr1, expr2) -> Sast.Address_t( (build_expr expr1), (build_expr expr2) ) 
  	| Ast.Call(str, expr_list) -> Sast.Call_t( str, (build_expr_list expr_list) )
 	| Ast.Noexpr -> Sast.Noexpr_t

and build_expr_list expr_list = 
	match expr_list with
	[] -> []
	| hd::tl -> let sast_expr_list = (build_expr hd) in sast_expr_list::(build_expr_list tl)

let rec build_stmt = function
	  Ast.Block(stmt_list) -> Sast.Block_t( (build_stmt_list stmt_list) )
	| Ast.Expr(expr) -> Sast.Expr_t( (build_expr expr) )
	| Ast.Return(expr) -> Sast.Return_t( (build_expr expr) )
	| Ast.Print(expr) -> Sast.Print_t( (build_expr expr) )
	| Ast.If(expr, stmt1, stmt2) -> Sast.If_t( (build_expr expr), (build_stmt stmt1), (build_stmt stmt2) )
	| Ast.For(expr1, expr2, expr3, stmt) -> Sast.For_t( (build_expr expr1), (build_expr expr2), (build_expr expr3), (build_stmt stmt) )
	| Ast.Loop(expr, stmt) -> Sast.Loop_t( (build_expr expr), (build_stmt stmt) )
	| Ast.While(expr, stmt) -> Sast.While_t( (build_expr expr), (build_stmt stmt) )
	| Ast.Vdecl( vardecl ) -> Sast.Vdecl_t( {vType_t=(ast_to_sast_type vardecl.vType); vName_t=vardecl.vName;} )
	| Ast.Vinit(decl, expr) -> Sast.Vinit_t( {vType_t=(ast_to_sast_type decl.vType); vName_t=decl.vName;} , (build_expr expr) )

and build_stmt_list stmt_list = 
	match stmt_list with
	[] -> []
	| hd::tl -> let sast_stmt_list = (build_stmt hd) in sast_stmt_list::(build_stmt_list tl) (* returns SAST body which is a SAST stmt list *)

(* let sc_stmt_list func env = 
	(* match func.body with
	[] -> []
	| _ ->  *) (* ignore type_stmt_list func env func.body; *) 
	build_stmt_list func.body *)

let is_id expr = 
	match expr with
	  Ast.Id(i) -> []
	| _ -> raise (Failure ("Mismatch Expression type: \n" ^ 
	    		"LHS of assign must be of type ID."))
	
let rec match_expr_list_types env types_list expr_list = 
	match expr_list with
		  [] -> (match types_list with
					[] -> []
					| _ -> raise (Failure ("Mismatch arguments number: function expects more arguments than supplied.")))
		| hd::tl -> ignore (type_expr (List.hd types_list) env hd); match_expr_list_types env (List.tl types_list) tl
			
and type_call typestring env name_str expr_list =
	let func_types_list = try StringMap.find name_str env.functions
						  with Not_found -> raise (Failure ("Undefined function: " ^ name_str)) 
		in
		let rtype = (List.hd func_types_list) in
		if rtype != typestring && typestring <> "any"
		then raise (Failure ("Mismatch Expression type: \n" ^ 
			     	"function has return type " ^ rtype ^ ".\n" ^
			   		"an expression of type " ^ typestring ^ " was expected."))
		else match_expr_list_types env (List.tl func_types_list) expr_list 

and type_binop typestring env expr1 op expr2 =
	match op with
	  Ast.Add -> ignore (type_expr "double" env expr1); 
	  			 ignore (type_expr "double" env expr2);
	  			 "double"
	| Ast.Sub -> ignore (type_expr "double" env expr1); 
	  			 ignore (type_expr "double" env expr2);
	  			 "double"
	| Ast.Mult -> ignore (type_expr "double" env expr1); 
	  			  ignore (type_expr "double" env expr2);
	  			  "double"
	| Ast.Div -> ignore (type_expr "double" env expr1); 
	  			 ignore (type_expr "double" env expr2);
	  			 "double"
	(* TODO boolean should take note or chord or track too*)
	| Ast.Equal -> ignore (type_expr "primitive" env expr1); 
	  			   ignore (type_expr "primitive" env expr2);
	  			   "boolean"
	| Ast.Neq -> ignore (type_expr "primitive" env expr1); 
	  			 ignore (type_expr "primitive" env expr2);
	  			 "boolean"
	| Ast.Geq -> ignore (type_expr "primitive" env expr1); 
	  			 ignore (type_expr "primitive" env expr2);
	  			 "boolean"
	| Ast.Leq -> ignore (type_expr "primitive" env expr1); 
	  			 ignore (type_expr "primitive" env expr2);
	  			 "boolean"
	| Ast.Greater -> ignore (type_expr "primitive" env expr1); 
	  			     ignore (type_expr "primitive" env expr2);
	  			     "boolean"
	| Ast.Less -> ignore (type_expr "primitive" env expr1); 
	  			  ignore (type_expr "primitive" env expr2);
	  			  "boolean"
	 (* TODO either has to be chord OR note OR track *)
	| Ast.Ser -> (match typestring with
	  			 | "track" ->
						ignore (type_expr "track" env expr1); 
		 	  			ignore (type_expr "chord" env expr2);
		 	  			"track"
	  			 | "any" ->
						ignore (type_expr "track" env expr1); 
		 	  			ignore (type_expr "chord" env expr2);
		 	  			"track"
	  			 | _ -> raise (Failure ("Mismatch Expression type: \n" ^ 
  						     	"expression was of type track.\n" ^
  						   		"but an expression of type " ^ typestring ^ " was expected.")) )
	 (* TODO either has to be chord OR note OR track OR score *)
	| Ast.Par -> (match typestring with
	  			   "score" ->
		  				ignore (type_expr "score" env expr1); 
		   	  			ignore (type_expr "track" env expr2);
		   	  			"score"
	  			 | "chord" -> 
		 				ignore (type_expr "chord" env expr1); 
		  	  			ignore (type_expr "chord_or_note_or_rest" env expr2);
		  	  			"chord"	
	  			 | "any" -> (try
								ignore (type_expr "chord" env expr1); 
				 	  			ignore (type_expr "chord_or_note_or_rest" env expr2);
				 	  			"chord"
				 	  		with Failure cause -> 
						 	  		try 
			 	  						ignore (type_expr "score" env expr1); 
			 	  		 	  			ignore (type_expr "score_or_track" env expr2);
			 	  		 	  			"score"
			 	  		 	  		with Failure cause -> raise (Failure ("Mismatch Expression type: \n" ^ 
	  				  						     	"expression was required to be of type score or chord.\n" ^
	  				  						   		"but an expression of type " ^ typestring ^ " was expected.")) )
			 	| _ -> raise (Failure ("Mismatch Expression type: \n" ^ 
                       "expression was required to be of type score or chord.\n" ^
                       "but an expression of type " ^ typestring ^ " was expected.")) )
	
and type_expr typestring env expr =
	match expr with
	  Ast.Literal(i) -> if typestring <> "double" && typestring <> "any" && typestring <> "primitive"
  						then raise (Failure ("Mismatch Expression type: \n" ^ 
  						     	"expression was of type double.\n" ^
  						   		"an expression of type " ^ typestring ^ " was expected."))
	  					else env
    | Ast.Id(i) -> let id_type = get_variable_type i env in
    				if typestring = "primitive"
    				then
	    				if id_type <> "note" && id_type <> "chord" && id_type <> "track" && id_type <> "score" && id_type <> "double"
						then raise (Failure ("Mismatch Expression type: \n" ^ 
						     	"expression was of type " ^ id_type ^ ".\n" ^
						   		"an expression of type " ^ typestring ^ " was expected."))
						else env
	    			else (match typestring with
		    				 "any" -> env
		    				| "chord_or_note_or_rest" -> (match id_type with
		    												  "chord" -> env
		    												| "note" -> env
		    												| "rest" -> env
		    												| _ -> raise (Failure ("Mismatch Expression type: \n" ^ 
														     	"expression was of type " ^ id_type ^ ".\n" ^
														   		"an expression of type " ^ typestring ^ " was expected.")) )
		    				| "score_or_track" -> (match id_type with
    												  "score" -> env
    												| "track" -> env
    												| _ -> raise (Failure ("Mismatch Expression type: \n" ^ 
												     	"expression was of type " ^ id_type ^ ".\n" ^
												   		"an expression of type " ^ typestring ^ " was expected.")) )
		    				| "track_or_chord" -> (match id_type with
    												  "track" -> env
    												| "chord" -> env
    												| _ -> raise (Failure ("Mismatch Expression type: \n" ^ 
												     	"expression was of type " ^ id_type ^ ".\n" ^
												   		"an expression of type " ^ typestring ^ " was expected.")) )
		    				| _ -> if typestring <> id_type
				    				then raise (Failure ("Mismatch Expression type: \n" ^ 
								     	"expression was of type " ^ id_type ^ ".\n" ^
								   		"an expression of type " ^ typestring ^ " was expected.")) 
					    			else env )
	| Ast.ACCESSOR(expr, note_attr) -> ignore (type_expr "note" env expr);
										if typestring <> "double" && typestring <> "any"
				  						then raise (Failure ("Mismatch Expression type: \n" ^ 
				  						     	"expression was of type double.\n" ^
				  						   		"an expression of type " ^ typestring ^ " was expected."))
					  					else env
	| Ast.NOTE_CR(expr1, expr2, expr3) -> if typestring <> "primitive" && typestring <> "note" && typestring <> "chord_or_note_or_rest" && typestring <> "any"
										  then raise (Failure ("Mismatch Expression type: \n" ^ 
				  						     	"expression was of type note.\n" ^
				  						   		"an expression of type " ^ typestring ^ " was expected."))
										  else ignore (type_expr "double" env expr1);
											   ignore (type_expr "double" env expr2);
											   ignore (type_expr "double" env expr3);
											   env
	| Ast.REST_CR(expr) -> if typestring <> "primitive" && typestring <> "rest" && typestring <> "chord_or_note_or_rest" && typestring <> "any"
						   then raise (Failure ("Mismatch Expression type: \n" ^ 
  						      	"expression was of type rest.\n" ^
  						   		"an expression of type " ^ typestring ^ " was expected."))
						   else ignore (type_expr "double" env expr);
							    env
	| Ast.CHORD_CR(expr_list) -> if typestring <> "primitive" && typestring <> "chord" && typestring <> "chord_or_note_or_rest" && typestring <> "track_or_chord" && typestring <> "any"
								 then raise (Failure ("Mismatch Expression type: \n" ^ 
		  						    "expression was of type chord.\n" ^
		  						   	"an expression of type " ^ typestring ^ " was expected."))
								 else ignore (type_expr_list "chord_or_note_or_rest" env expr_list);
								 	  env
	| Ast.TRACK_CR(expr) -> if typestring <> "primitive" && typestring <> "track" && typestring <> "track_or_chord" && typestring <> "score_or_track" && typestring <> "any"
								 then raise (Failure ("Mismatch Expression type: \n" ^ 
		  						    "expression was of type track.\n" ^
		  						   	"an expression of type " ^ typestring ^ " was expected."))
								 else ignore (type_expr "double" env expr);
								 	  env
	| Ast.SCORE_CR(expr_list) -> if typestring <> "primitive" && typestring <> "score" && typestring <> "score_or_track" && typestring <> "any"
								 then raise (Failure ("Mismatch Expression type: \n" ^ 
		  						    "expression was of type track.\n" ^
		  						   	"an expression of type " ^ typestring ^ " was expected."))
								 else ignore (type_expr_list "track" env expr_list);
								 	  env	
	| Ast.Binop(expr1, op, expr2) -> let binop_type = type_binop typestring env expr1 op expr2 in
										if typestring <> binop_type && typestring <> "any"
				  						then raise (Failure ("Mismatch Expression type: \n" ^ 
			  						            "expression was of type " ^ binop_type ^ ".\n" ^
				  						   		"an expression of type " ^ typestring ^ " was expected."))
					  					else env
	| Ast.Modifier(expr, m) -> ignore (type_expr "primitive" env expr); env
	| Ast.Assign(expr1, expr2) -> ignore (is_id expr1);
								  ignore (type_expr typestring env expr1);
								  ignore (type_expr typestring env expr2);
								  (* TODO update environment with initialized boolean *)
								  (* TODO update environment? *)
								  env
	| Ast.Address(expr1, expr2) -> (match typestring with
									"track" -> ignore (type_expr "score" env expr1);
											   ignore (type_expr "double" env expr2);
											   env
									| "chord" -> ignore (type_expr "track" env expr1);
											     ignore (type_expr "double" env expr2);
											     env
									| "note" -> ignore (type_expr "chord" env expr1);
											    ignore (type_expr "double" env expr2);
											    env
									| "rest" -> ignore (type_expr "chord" env expr1);
											    ignore (type_expr "double" env expr2);
											    env
									| "any" -> ignore (type_expr "any" env expr1);
											   ignore (type_expr "double" env expr2);
											   env
									| _ -> raise (Failure ("Mismatch Expression type: \n" ^ 
			  						            "expression was of the wrong type.\n" ^
				  						   		"an expression of type " ^ typestring ^ " was expected.")) )
  	| Ast.Call(name_str, expr_list) -> ignore (type_call typestring env name_str expr_list); 
  										env
 	| Ast.Noexpr -> env

and type_expr_list typestring env = function
		[] -> []
	| hd::tl -> let new_env = (type_expr typestring env hd) in type_expr_list typestring new_env tl
	

(* function matches a STATEMENT *)
(*|| func.fname = "Song"*)
let rec type_stmt func env stmt =
	match stmt with 
	  Ast.Block(stmt_list) -> type_stmt_list func env stmt_list
	| Ast.Expr(expr) -> type_expr "any" env expr
	| Ast.Print(expr) -> type_expr "any" env expr
	| Ast.Return(expr) -> if func.fname != "song" 
							then type_expr (string_of_vartype func.rtype) env expr
						  else
							 let rtn_type = string_of_vartype func.rtype in
							 	if rtn_type != "score" 
							 	then raise (Failure ("Return type of song function must be of type score."))
								else type_expr (string_of_vartype func.rtype) env expr
	(* reordered! expr comes last (after stmts) becuase its the only one that can change the environment outside the block *)
	| Ast.If(expr, stmt1, stmt2) -> ignore (type_stmt func env stmt1);
									ignore (type_stmt func env stmt2);
									type_expr "boolean" env expr
	(* expr1=assign, expr2=boolean, expr3=junk *)
	| Ast.For(expr1, expr2, expr3, stmt) -> let for_env = type_expr "double" env expr1 in 
												ignore (type_expr "any" for_env expr2);
												ignore (type_expr "any" for_env expr3);
												ignore (type_stmt func for_env stmt);
												env
	| Ast.Loop(expr, stmt) -> let loop_env = type_expr "double" env expr in 
								ignore (type_stmt func loop_env stmt);
								env
	| Ast.While(expr, stmt) -> let while_env = type_expr "boolean" env expr in 
								ignore (type_stmt func while_env stmt);
								env
	| Ast.Vdecl(vardecl) -> let new_locals_stringmap = add_local vardecl.vType vardecl.vName env in
								let new_env = 
									{
										locals = new_locals_stringmap; 
										globals = env.globals; 
										functions = env.functions 
									} in
									new_env
	| Ast.Vinit(vardecl, expr) -> let new_locals_stringmap = add_local vardecl.vType vardecl.vName env in
									let new_env = 
										{
											locals = new_locals_stringmap; 
											globals = env.globals; 
											functions = env.functions 
										} in
										type_expr (string_of_vartype vardecl.vType) new_env expr
										
and type_stmt_list func env = function
	  [] -> env
	| hd::tl -> let new_env = (type_stmt func env hd) in type_stmt_list func new_env tl
	

(* let rec sc_local_vars func env =  *)

(* check the expression type can be used for
 * the corresponding argument according to definition
 * return the new expression list in expr_t for sast *)
let sc_func_arg lst expr arg_t =
	if (snd expr) = arg_t then (fst expr)::lst else
	raise (Failure("function arguments do not match"))
(* FUNCTIONS  - EMILY *)
(*checks function arguments, then updates env*)
let sc_formal formal env =
	(*fstrently, formals are var_decls*)
	let new_locals_stringmap = add_local formal.vType formal.vName env in
		let env = 
		{
			locals = new_locals_stringmap; 
			globals = env.globals; 
			functions = env.functions 
		} in
	convert_types formal, env
(* check function arguments *)


(* updates formals from fst context *)
(* in = function formals + env *)
let rec sc_formals formals env =
	match formals with
	  [] -> []
	| h::t -> let f, new_env = (sc_formal h env) in (f, new_env)::(sc_formals t new_env) 


(* sc_function
	returns updated formals + body
	returns type, name, locals
 *)
let rec sc_function fn env = 
	match List.hd (List.rev fn.body) with
		(* check there is a return statement at the end of the function *)
		(* TODO only song needs a return! *)
		Return(_) -> 
			(* updating this function's personal envirnment *)
			(* let env = 
				{
					locals = StringMap.empty;
					globals = env.globals;
					functions = env.functions;
				}
			fill up env_new with functions;
			change name possibly to something more intuitive
			new_fn_sm - new function stringmap
			
			in *)
			let new_function_stringmap = add_function fn.fname fn.rtype fn.formals env in
				let env =
					{
						locals = StringMap.empty;
						globals = env.globals;
						functions = new_function_stringmap (* new function env *)
					} 
				
			(* check formal arguments with sc_formals 
			formals_env
				- returns formal list appended w/ new environment as tuples
			*)
			in
			let function_environment_tuple_list = sc_formals fn.formals env in (* f is tuple (formals, env) *)
				(* formals = list of formals *)
				let formals_list = List.map (fun formal -> fst formal ) function_environment_tuple_list in
				(match formals_list with
					(* empty, no formals *)
					[] -> ignore (type_stmt_list fn env fn.body);
							let sast_body = build_stmt_list fn.body in
								{
									Sast.rtype_t = ast_to_sast_type fn.rtype;
									Sast.fname_t = fn.fname;
									Sast.formals_t = formals_list; (* ie empty *)
									Sast.body_t = sast_body
								}, env
					|_ -> let new_env = snd (List.hd (List.rev function_environment_tuple_list)) in
							ignore (type_stmt_list fn new_env fn.body);
							let sast_body = build_stmt_list fn.body in
								{
									Sast.rtype_t = ast_to_sast_type fn.rtype;
									Sast.fname_t = fn.fname;
									Sast.formals_t = formals_list; (* ie empty *)
									Sast.body_t = sast_body
								}, new_env
				)
		|_ -> raise (Failure ("The last statement must be a return statement"))
			(*let f = sc_formals fn.formals env i stopped fu nv stuff at ln 196*)


(* check function list *)
let rec sc_functions fns env =
	match fns with
	(* if no functions, return empty list *)
	[] -> []
	(* otherwise, go through and create a list of function, environment
	pairs; the last element in the list is the most up-to-date env *)
	| h::t -> let f, e = (sc_function h env) in f::(sc_functions t e)


(* TOM - I don't know what this is so I didn't want to change it *)
(*invokes a function and returns updated formals and block from env. Needs to also
update the symbol table for global variables*)
(*let functions_checker env func =

let rec functions_update env funcs = 
*)


(* GLOBALS - EMILY *)

(* sem check global *)
let sc_global global env = 
	(* add_global returns updated stringmap *)
	let new_global_stringmap = add_global global.vType global.vName env in
		let env = 
			{
				locals = env.locals; 
				globals = new_global_stringmap;
				functions = env.functions
			} in
		(* 
		RETURN: global + env
		*)
		convert_types global, env 

(* sem check list of globals *)
let rec sc_globals globals env =
	match globals with
	(* empty list of globals*)
	[] -> [] 
	(* 
		- iterate through list of globals 
		- semantically check each individual global
		- (g, e) end up being pairs of globals + respective environments
		- the last (g,e) pair has env with info from all globals
	*)
	| h::t -> let g, e = (sc_global h env) in (g,e)::(sc_globals t e)


(* semantically check program - Emily *)
let sc_program (globals, functions) =
	(*  initialize empty env *)
	let env = 
		{ 	locals 	  = StringMap.empty;
			globals   = StringMap.empty;
			functions = StringMap.empty 
		} in 
	(* 
		sc_globals returns list: [(g1,e1), (g2,e2), (g3,e3)....(gn,en)]
		where g = global , e = environment
	*)
	let g = sc_globals globals env in
		(* make a list of globals *)
		(* note: fun = function pattern matching *)
		(* note: elementss returned are in form (g, e)
			-fst global returns g
			-snd global returns e
		*)

		let globals = List.map (fun global -> fst global) g in
			match g with
				(* no globals; thus our environment stays the same *)
				[] -> (globals, (sc_functions (List.rev functions) env))
				| _ -> let new_env = snd (List.hd (List.rev g)) in 
						(*let new_functions = (fst(List.rev (sc_functions (List.rev functions) new_env))) in
							new_globals,  new_functions*)
						(globals, (sc_functions (List.rev functions) new_env))


(*         let globals = List.map (fun global -> fst global) g in
        match g with
        (* no globals *)
         [] -> (globals, (check_functions env (List.rev funcs)))
        (* get the envirnment from the last global *)
        | _ -> let e = snd (List.hd (List.rev g)) in (globals, (check_functions e (List.rev funcs)))


 *)


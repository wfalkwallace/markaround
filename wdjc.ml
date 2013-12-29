type action = Ast | Compile | Java | Sast

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ 
            ("-a", Ast);
            ("-s", Sast);
            ("-j", Java);
            ("-c", Compile) ]
  else Compile in
  
    let output_name = 
      if Array.length Sys.argv > 2 then
        Sys.argv.(2) 
      else "song" in
  
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
      Ast -> let listing = Ast.string_of_program program
          in print_string listing
    | Sast -> let program_t = Semcheck.sc_program program in 
          let listing = Sast.string_of_program_t program_t
          in print_string listing
    | Java -> let listing = Compile.string_of_program output_name (Semcheck.sc_program program) in
          (* let compile = Sys.command("javac -classpath tests:java/jMusic/jMusic1.6.4.jar:java/jMusic/inst/:. tests/" ^ output_name ^ "dj.java") in
            print_int compile; *)
          print_endline listing
    | Compile -> let listing = Compile.string_of_program output_name (Semcheck.sc_program program) in 
          (* let output = Sys.command("./compile " ^ output_name ^ "dj") in *)
          (* let compile = Sys.command("javac -classpath tests:java/jMusic/jMusic1.6.4.jar:java/jMusic/inst/:. tests/" ^ output_name ^ "dj.java") in
          let run = Sys.command("java -classpath tests:java/jMusic/jMusic1.6.4.jar:java/jMusic/inst/:. " ^ output_name ^ "dj") in *)
            (* print_endline listing;  *)
            ignore( listing );
            print_int 0
open Parser
open Lexer
open Types
open Evaluator
open Shuffle

(* 
 * reading database from input files.
 * params - program parameters
 *)
let read_database params = 
 
    let database = ref []           (* database we create *)
    in          

    (* 
     * function reading a file and extendind actual database 
     * filename: file to read from.
     *)

    let extend_database filename =                          
        let read_file source =
            let buffer = ref "" 
            in 
            try
                while (true) do
                    buffer := !buffer ^ (input_line source) ^ "\n"
                done;
                !buffer (* dummy *) 
            with 
                | End_of_file -> !buffer
        in    
            
        try
            let source      = open_in filename in           (* input channel *)
            let buffer      = read_file source in
               begin
                 close_in source;
                  
	         database := (Parser.clause_list Lexer.token (Lexing.from_string buffer)) @ !database;
               end
        with 
            | Sys_error s ->    (* case of system error *)
                print_endline ((Filename.basename filename)^ ": " ^ s)                                                           
            |   e       ->    (* handling other cases *)
                print_endline ((Filename.basename filename) ^ ": " ^ " Error occurred.");
				raise e
    in
    begin
        let parameters = match Array.length params with              (* the first argument of the program is it's name *)
                            |  1  -> Array.make 0 ""                 (* omit the first parameter - case it's the only one *)
                            |  i  -> Array.sub params 1 (i - 1)      (* omit the first parameter - case of more than 1 parameter *)
        in
            Array.iter (fun filename -> if Sys.file_exists filename         (* check if the file in param exists in filesystem *)
                                        then extend_database filename       (* try to extend database with file contents *)
                                        else print_endline                  (* warn user *)
                                                ("File " ^ (Filename.basename filename) ^ " does not exist."))   
                        parameters;
            !database                                           (* return created database *)
    end



(*
 *  main interpreter function.
 *)
let main () =

    let database = read_database Sys.argv
    in 
    	try 
			while (true) 				(* while not EOF *)
			do	
	    		print_string ":- ";
                flush stdout;

                try
					let db = shuffle database
						(* read the expression *)
					and	buff = Lexing.from_string(read_line())
						(* create its syntax tree*)
					in
						interpret (Parser.query Lexer.token buff) db false false;
                with
                    | Failure ("lexing: empty token")    (* lexing failure *)
                    | Parsing.Parse_error ->             (* parsing failure *)
                        print_endline "Parse error. Did you forget a dot?"
                    | Failure s -> print_endline ("Failed: " ^ s) 
			done
		with
			| End_of_file -> (print_string "\n"; exit 0)
            |      _      -> (print_endline "Error occured."; exit 0)
				
				
let _ = 
	Random.self_init ();
	main ()  
             

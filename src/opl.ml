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
        let parameters = match Array.length params with
            |  1  -> Array.make 0 ""
            |  i  -> Array.sub params 1 (i - 1)
        in
            Array.iter (fun filename -> 
				if Sys.file_exists filename
                then extend_database filename
                else print_endline ("File " ^ (Filename.basename filename) ^ " does not exist.")) parameters;
            !database
    end

let prompt () =
	print_string ":- "; 
	flush stdout

let read_eval_print database =
	prompt ();

    try	let db = shuffle database
	    and buff = Lexing.from_string (read_line ())
		in
			interpret (Parser.query Lexer.token buff) db false false;
    with
        | Failure ("lexing: empty token")    (* lexing failure *)
        | Parsing.Parse_error ->             (* parsing failure *)
            print_endline "Parse error. Did you forget a dot?"
        | Failure s -> print_endline ("Failed: " ^ s) 

(*
 *  main interpreter function.
 *)
let main () =

    let database = read_database Sys.argv
    in 
    	try 
			let rec main_loop_body () =
				read_eval_print database; (* exception on EOF *)
				main_loop_body ()
			in 
				main_loop_body ()
		with
			| End_of_file -> (print_string "\n"; exit 0)
            | _           -> (print_endline "Error occurred."; exit 0)

let _ = 
	Random.self_init ();
	main ()  
             

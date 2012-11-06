open Parser
open Lexer
open Types
open Evaluator
open Shuffle

(* http://pleac.sourceforge.net/pleac_ocaml/filecontents.html *)
let slurp_channel channel =
	let buffer_size = 4096 in
	let buffer = Buffer.create buffer_size in
	let string = String.create buffer_size in
	let chars_read = ref 1 in
		while !chars_read <> 0 do
			chars_read := input channel string 0 buffer_size;
			Buffer.add_substring buffer string 0 !chars_read
		done;
		Buffer.contents buffer
			
let slurp_file filename =
	let channel = open_in_bin filename in
	let result =
		try slurp_channel channel
		with e -> close_in channel; raise e in
		close_in channel;
		result

(* 
 * reading database from input files.
 * params - program parameters
 *)
let read_database params = 
 
    let database = ref []           (* database we create *)
    in          

    let extend_database filename =                          
		let base_filename = Filename.basename filename
		in
			try
				let buffer = slurp_file filename in
					database := (Parser.clause_list Lexer.token 
									 (Lexing.from_string buffer)) @ !database
			with 
				| Sys_error s -> 
					print_endline (base_filename ^ ": " ^ s)
				| e -> 
					print_endline (base_filename ^ ": " ^ " Error occurred.");
					raise e
    in
    let parameters = match Array.length params with
        |  1  -> Array.make 0 ""
        |  i  -> Array.sub params 1 (i - 1)
    in
	let process_file filename =
		if Sys.file_exists filename
        then extend_database filename
        else print_endline ("File " ^ (Filename.basename filename) ^ " does not exist.")
	in
        Array.iter process_file parameters;
        !database

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

let repl database = 
    try 
		let rec main_loop_body () =
			read_eval_print database; (* exception on EOF *)
			main_loop_body ()
		in 
			main_loop_body ()
	with
		| End_of_file -> (print_string "\n"; exit 0)
        | _           -> (print_endline "Error occurred."; exit 0)
			
(*
 *  main interpreter function.
 *)
let main () =

    let database = read_database Sys.argv
    in repl database

let _ = 
	Random.self_init ();
	main ()  
             

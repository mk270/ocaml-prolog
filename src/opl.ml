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
    let clauses_from_file filename =                          
		let base_filename = Filename.basename filename
		in
			try
				let buffer = slurp_file filename in
					Parser.clause_list Lexer.token 
									 (Lexing.from_string buffer)
			with 
				| e -> 
					print_endline (base_filename ^ ": " ^ " Error occurred.");
					raise e
	in
        List.flatten (List.map clauses_from_file params)

let prompt () =
	print_string ":- "; 
	flush stdout

let read_eval_print database randomise interactive quiet limit =
	let one_shot = limit > 0 in

	prompt ();

    try	
	    let buff = Lexing.from_string (read_line ()) in
		let query_term = Parser.query Lexer.token buff in
		let execute () =
			interpret query_term database interactive one_shot randomise quiet
		in
		let rec execute_many = function
			| 0 -> ()
			| n -> 
				execute ();
				execute_many (n - 1)
		in
			if not one_shot
			then execute_many 1
			else execute_many limit
    with
        | Failure ("lexing: empty token")    (* lexing failure *)
        | Parsing.Parse_error ->             (* parsing failure *)
            print_endline "Parse error. Did you forget a dot?"
        | Failure s -> print_endline ("Failed: " ^ s) 

let repl database randomise interactive quiet limit = 
    try 
		let rec main_loop_body () =
			(* exception on EOF *)
			read_eval_print database randomise interactive quiet limit; 
			main_loop_body ()
		in 
			main_loop_body ()
	with
		| End_of_file -> (print_string "\n"; exit 0)
        | _           -> (print_endline "Error occurred."; exit 0)

let main () =
	let filenames = ref [] in
	let randomise = ref false in
	let interactive = ref true in
	let quiet = ref false in
	let limit = ref 0 in
	let specs = [
		("-r", Arg.Set randomise, "randomise");
		("-n", Arg.Clear interactive, "non-interactive");
		("-q", Arg.Set quiet, "quiet");
		("-l", Arg.Set_int limit, "limit");
	]
	and add_to_filenames = fun s -> filenames := s :: !filenames 
	in
		Arg.parse specs add_to_filenames "Usage: opl filename1 filename2 ...";

		if !randomise
		then Random.self_init ()
		else ();

		repl (read_database !filenames) !randomise !interactive !quiet !limit

let _ = 
	main ()  
             

(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Shuffle
open Interpreter

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let clauses_from_file filename =                          
	let base_filename = Filename.basename filename in
		try
			load_file filename |> Interpreter.clauses_from_string
		with 
			| e -> 
				print_endline (base_filename ^ ": " ^ " Error occurred.");
				raise e

(* 
 * reading database from input files.
 * params - program parameters
 *)
let read_database params = 
    List.map clauses_from_file params |> List.flatten

let prompt () =
	print_string ":- "; 
	flush stdout

let read_eval_print database behaviour =
	prompt ();

    try	
		let query_term = read_line () |> Interpreter.term_from_string in
		let execute () =
			Interpreter.interpret query_term database behaviour
		in
		let rec execute_many = function
			| 0 -> ()
			| n -> 
				execute ();
				execute_many (n - 1)
		in
			(match behaviour.limit with
			| None -> 1
			| Some n -> n) 
			|> execute_many
    with
        | Failure ("lexing: empty token")    (* lexing failure *)
        | Parsing.Parse_error ->             (* parsing failure *)
            print_endline "Parse error. Did you forget a dot?"
        | Failure s -> print_endline ("Failed: " ^ s) 

let repl database behaviour = 
    try 
		let rec main_loop_body () =
			(* exception on EOF *)
			read_eval_print database behaviour; 
			main_loop_body ()
		in 
			main_loop_body ()
	with
		| End_of_file -> (print_string "\n"; exit 0)
        | _           -> (print_endline "Error occurred."; exit 0)

let set_limit = function
	| 0 -> None
	| n -> Some n

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

		let behaviour = {
			randomise = !randomise;
			interactive = !interactive;
			quiet = !quiet;
			limit = set_limit !limit;
		} in

		repl (read_database !filenames) behaviour

let _ = 
	main ()  

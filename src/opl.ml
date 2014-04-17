(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Interpreter

let get_limit = function
	| None -> 1
	| Some n -> n

let set_limit = function
	| 0 -> None
	| n -> Some n

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let read_database params = 
	List.map load_file params |>
    String.concat "" |>
    Interpreter.clauses_from_string

let prompt () =
	print_string ":- "; 
	flush stdout

let rec repeat thunk = function
	| 0 -> ()
	| n -> thunk (); repeat thunk (n - 1)

let rec loop_forever thunk =
	thunk ();
	loop_forever thunk

let read_eval_print database behaviour =
	prompt ();

    try	
		let query_term = read_line () |> Interpreter.term_from_string in
		let execute () =
			Interpreter.interpret database behaviour query_term
		in
			get_limit behaviour.limit |> repeat execute
    with
        | Failure ("lexing: empty token")    (* lexing failure *)
        | Parsing.Parse_error ->             (* parsing failure *)
            print_endline "Parse error. Did you forget a dot?"
        | Failure s -> print_endline ("Failed: " ^ s) 

let repl database behaviour = 
    try 
		loop_forever (fun () -> read_eval_print database behaviour)
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

		let behaviour = {
			randomise = !randomise;
			interactive = !interactive;
			quiet = !quiet;
			limit = set_limit !limit;
		} in

		repl (read_database !filenames) behaviour

let _ = 
	main ()  

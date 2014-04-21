(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Types
open Unification
open Var

type interp_behaviour = {
	randomise : bool;
	interactive : bool;
	quiet : bool;
	limit : int option;
}

type database = clause list

exception Not_integer


(* evaluates all possible ways a term given a specific database *)
let interpret database behaviour term = 
	let interactive = behaviour.interactive
	and one_shot = Limit.bool_of behaviour.limit
	and randomise = behaviour.randomise
	and	quiet = behaviour.quiet in
	let thunk = fun () -> () 
	and print_no = fun () -> 
		if quiet
		then ()
		else print_string "No\n"
	and print_yes = fun () ->
		if quiet
		then ()
		else print_string "Yes\n"
	in

	(* asks if evaluation should be continued *)
	let more () =
		if one_shot
		then false
		else if interactive
        then (print_string "More?\n";
		      if read_line() = ";" 
              then true
		      else false)
	    else true
	and filter replacement = 
		(* filters replacement that only variables that exist in term remains *)
		let variables = (Evaluator.get_variables term []) in
			List.filter 
				(fun (var,_) -> List.exists 
					(fun v -> v = var) variables) replacement
	in 
	let interact = fun vt fc -> 
		if fst vt 
		then (print_yes ();
			  Repr.print_replacement (filter (snd vt)); 
			  print_endline "";
			  if more () 
			  then fc () 
			  else ()
		)
		else fc ()
	in
	let database = Evaluator.maybe_shuffle randomise database in
		Evaluator.evaluate term database [] database 
			interact print_no thunk randomise

let clauses_from_string s =
	match (String.length s) with
	| 0 -> []
	| _ -> Lexing.from_string s |> Parser.clause_list Lexer.token 

let term_from_string s =
	Lexing.from_string s |> Parser.query Lexer.token

let interpret_string database behaviour s =
	term_from_string s |> interpret database behaviour

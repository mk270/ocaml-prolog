(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Types

exception Unsuported_term
exception Type_error

(* makes a string of term *)
let string_of_term term =
	let rec string_of_arguments = function
		| [] -> ""
		| [t] -> (string_of_term t)
		| t::terms -> (string_of_term t) ^ "," ^ (string_of_arguments terms)
	and string_of_term = function
		| TermVariable v -> v
		| TermString str -> str
		| TermConstant const -> (match const with
				| ConstantAtom str -> str
				| ConstantNumber num -> 
					(match num with
						| Integer n -> string_of_int n))
		| TermFunctor(nam,args) ->
			nam ^ "(" ^ (string_of_arguments args) ^ ")"
		| TermIfThen (t1, t2) -> "if " ^ (string_of_terms_with_sep " then " t1 t2)
		| TermIfThenElse (t1, t2,t3) -> "if " ^ (string_of_terms_with_sep " then " t1 t2) ^ " else " ^ (string_of_term t3)
		| TermNegation t -> "not " ^ (string_of_term t)
		| TermIs (t1, t2) -> (string_of_terms_with_sep " is " t1 t2)
		| TermOr (t1, t2) -> (string_of_terms_with_sep "; " t1 t2)
		| TermAnd (t1, t2) -> (string_of_terms_with_sep ", " t1 t2)
		| TermArithmeticPlus (t1, t2) -> (string_of_terms_with_sep " + " t1 t2)
		| TermArithmeticMinus (t1, t2) -> (string_of_terms_with_sep " - " t1 t2)
		| TermArithmeticMult (t1, t2) -> (string_of_terms_with_sep " * " t1 t2)
		| TermArithmeticDiv (t1, t2) -> (string_of_terms_with_sep " / " t1 t2)
		| TermArithmeticEquality (t1, t2) -> (string_of_terms_with_sep " =:= " t1 t2)
		| TermArithmeticInequality (t1, t2) -> (string_of_terms_with_sep " =\\= " t1 t2)
		| TermArithmeticLess (t1, t2) -> (string_of_terms_with_sep " < " t1 t2)
		| TermArithmeticGreater (t1, t2) -> (string_of_terms_with_sep " > " t1 t2)
		| TermArithmeticLeq (t1, t2) -> (string_of_terms_with_sep " <= " t1 t2)
		| TermArithmeticGeq (t1, t2) -> (string_of_terms_with_sep " >= " t1 t2)
		| TermTermEquality (t1, t2) -> (string_of_terms_with_sep " == " t1 t2)
		| TermTermUnify (t1, t2) -> (string_of_terms_with_sep " = " t1 t2)
		| TermTermNotUnify (t1, t2) -> (string_of_terms_with_sep " /= " t1 t2)
		| TermList list ->
			(match list with
				| EmptyList -> "[]"
				| NormalList args -> "[" ^ (string_of_arguments args) ^ "]"
				| DividedList(args,term) -> "[" ^ (string_of_arguments args) ^ " | " ^ (string_of_term term) ^ "]")
		| _ -> ""
	and string_of_terms_with_sep sep t1 t2 = (string_of_term t1) ^ " " ^ sep ^ " " ^ (string_of_term t2)
	in
		string_of_term term


(* writes replacement to standard output *)
let rec print_replacement = function
	| [] -> ()
	| (v, rep_term) :: trep -> 
		print_string (v ^ " = " ^ (string_of_term rep_term) ^ "   "); 
		print_replacement trep


(* appends a replacement to a term *)
let rec replace term replacement = 
  let rep term' =                       (* used for maping in TermFunctor *)
      replace term' replacement
  in
	  match term with
		  | TermOr (t1,t2) -> TermOr (replace t1 replacement, replace t2 replacement)
		  | TermAnd (t1,t2) -> TermAnd (replace t1 replacement, replace t2 replacement)
		  | TermVariable var ->
			  (match replacement with
				  | [] -> term
				  | (v, rep) :: replacement' -> 
					  if v = var 
					  then rep   (* found current variable var in replacement *)
					  else replace term replacement') (* continues searching for variable *)
		  | TermFunctor (nam,args) -> TermFunctor (nam,List.map rep args)
		  | TermIs (t1,t2) -> TermIs (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticPlus (t1,t2) -> TermArithmeticPlus (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticMinus (t1,t2) -> TermArithmeticMinus (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticMult (t1,t2) -> TermArithmeticMult (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticDiv (t1,t2) -> TermArithmeticDiv (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticEquality (t1,t2) -> TermArithmeticEquality (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticInequality (t1,t2) -> TermArithmeticInequality (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticLess (t1,t2) -> TermArithmeticLess (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticGreater (t1,t2) -> TermArithmeticGreater (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticLeq (t1,t2) -> TermArithmeticLeq (replace t1 replacement, replace t2 replacement)
		  | TermArithmeticGeq (t1,t2) -> TermArithmeticGeq (replace t1 replacement, replace t2 replacement)
		  | TermTermEquality (t1,t2) -> TermTermEquality (replace t1 replacement, replace t2 replacement)
		  | TermTermUnify (t1,t2) -> TermTermUnify (replace t1 replacement, replace t2 replacement)
		  | TermTermNotUnify (t1,t2) -> TermTermNotUnify (replace t1 replacement, replace t2 replacement)
		  | TermNegation t -> TermNegation (replace t replacement)
		  | TermList list ->
			  (match list with
				  | EmptyList -> term
				  | NormalList args -> TermList (NormalList (List.map rep args))
				  | DividedList(args,term) -> TermList (DividedList (List.map rep args, rep term)))
		  | _ -> term


(* adds new variable replacement to given replacement *)
let rec add_replacement (var,term) replacement =
	let replace_rep (var',term') =
		(var',replace term' [(var,term)])   (* replaces variable var if it is in the replacement *)
	in
		(var,term)::(List.map replace_rep replacement) (* replaces var in every node of replacement *)
    
let fail_unify = (false, [])

(* tries to unify two terms, returns if terms can be unified and replacement needed for unification *)
let rec unify term1 term2 rep =
	let rec unify_args args1 args2 rep = (* unifies arguments of functors *)
		match args1, args2 with
			| [], _ -> (true,rep)
			| term1 :: terms1, [] -> raise Type_error
			| term1 :: terms1, term2::terms2 -> 
				let uni = unify term1 term2 rep
				in
					if fst uni 
					then unify_args terms1 terms2 (snd uni) 
					else fail_unify
	and divide_list list n =
		let rec divlist list1 list2 n =
			match list2 with
				| [] -> (list1,[])
				| hd::tl -> 
					if n = 0 
					then (list1,list2) 
					else divlist (hd::list1) tl (n-1)
		in
		let (list1,list2) = divlist [] list n
		in
			(List.rev list1, list2)  
	and rterm1 = replace term1 rep  (* append replacment for terms to unify *)
	and rterm2 = replace term2 rep

	and unify_mismatched_lists args_dl term args_nl nl_is_rterm2 =
		if (List.length args_dl) > (List.length args_nl)
		then fail_unify
		else
			let (args_nl',args_nl'') = divide_list args_nl (List.length args_dl)
			in
			let uni = 
				if nl_is_rterm2
				then unify_args args_nl' args_dl rep
				else unify_args args_dl args_nl' rep
			in
				if fst uni
				then unify (TermList (NormalList args_nl'')) term (snd uni)
				else fail_unify

	and unify_rterm2 rterm1 rterm2 = match rterm2, rterm1 with
		| TermVariable v2, _ -> (true,(add_replacement (v2,rterm1) rep))
		| TermAnd(t21,t22), TermAnd(t11,t12) 
		| TermOr(t21,t22), TermOr(t11,t12)
		| TermIs(t21,t22), TermIs(t11,t12)
		| TermArithmeticPlus(t21,t22), TermArithmeticPlus(t11,t12)
		| TermArithmeticMinus(t21,t22), TermArithmeticMinus(t11,t12)
		| TermArithmeticMult(t21,t22), TermArithmeticMult(t11,t12)
		| TermArithmeticDiv(t21,t22), TermArithmeticDiv(t11,t12)
		| TermArithmeticEquality(t21,t22), TermArithmeticEquality(t11,t12)
		| TermArithmeticInequality(t21,t22), TermArithmeticInequality(t11,t12)
		| TermArithmeticLess(t21,t22), TermArithmeticLess(t11,t12)
		| TermArithmeticGreater(t21,t22), TermArithmeticGreater(t11,t12)
		| TermArithmeticLeq(t21,t22), TermArithmeticLeq(t11,t12)
		| TermArithmeticGeq(t21,t22), TermArithmeticGeq(t11,t12)
		| TermTermEquality(t21,t22), TermTermEquality(t11,t12)
		| TermTermUnify(t21,t22), TermTermUnify(t11,t12)
		| TermTermNotUnify(t21,t22), TermTermNotUnify(t11,t12) ->
					let uni1 = unify t11 t21 rep in
						if fst uni1
						then unify t12 t22 (snd uni1)
						else fail_unify
		| TermNegation t2, TermNegation t1 -> unify t1 t2 rep
		| TermFunctor(nam2,args2), TermFunctor(nam1,args1) ->
					if nam1 = nam2 && (List.length args1) = (List.length args2)
					then unify_args args1 args2 rep
					else fail_unify
		| TermList (EmptyList), TermList (EmptyList) -> (true,rep)
		| TermList (EmptyList), TermList (NormalList []) -> (true,rep)
		| TermList (NormalList args2), TermList (EmptyList) ->
					if (List.length args2) = 0
					then (true,rep)
					else fail_unify
		| TermList (NormalList args2), TermList (NormalList args1) ->
					if (List.length args2) = (List.length args1)
					then unify_args args1 args2 rep
					else fail_unify
		| TermList (NormalList args_nl), TermList (DividedList (args_dl,term)) ->
			unify_mismatched_lists args_dl term args_nl true
		| TermList (DividedList(args_dl,term)), TermList (NormalList args_nl) ->
			unify_mismatched_lists args_dl term args_nl false
		| TermList (DividedList(args2,term2)), TermList (DividedList(args1,term1)) ->
					if (List.length args2) >= (List.length args1)
					then let (args2', args2'') = divide_list args2 (List.length args1)
						 in
						 let uni = unify_args args2' args1 rep in
							 if fst uni
							 then 
								 if (List.length args2'') = 0
								 then unify term1 term2 (snd uni)
								 else unify term1 (TermList (DividedList (args2'',term2))) (snd uni)
							 else fail_unify
					else
						let (args1', args1'') = divide_list args1 (List.length args2)
						in
						let uni = unify_args args2 args1' rep in
							if fst uni
							then unify (TermList (DividedList (args1'',term1))) term2 (snd uni)
							else fail_unify
		| TermAnd _, _
		| TermOr _, _
		| TermIs _, _
		| TermArithmeticPlus _, _
		| TermArithmeticMinus _, _
		| TermArithmeticMult _, _
		| TermArithmeticDiv _, _
		| TermArithmeticEquality _, _
		| TermArithmeticInequality _, _
		| TermArithmeticLess _, _
		| TermArithmeticGreater _, _
		| TermArithmeticLeq _, _
		| TermArithmeticGeq _, _
		| TermTermEquality _, _
		| TermTermUnify _, _
		| TermTermNotUnify _, _
		| TermNegation _, _
		| TermFunctor _, _
		| TermList _, _ -> fail_unify
		| rterm2', rterm1' -> 
			if rterm1' = rterm2'
			then (true,rep) 
			else fail_unify
	in
		if rterm1 = rterm2 
		then (true,rep)  (* terms are the same *)
		else
			match rterm1 with
				| TermVariable v1 -> (true,(add_replacement (v1,rterm2) rep)) (* left term is a variable so we add this variable to replacement (it will be replaced by right term *)
				| _ ->  (* left term is not a variable *)
					unify_rterm2 rterm1 rterm2








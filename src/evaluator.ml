open Types;;
open Unificator;;

exception Not_a_number;;
exception Cant_evaluate;;
exception Not_integer;;

let c = ref 0 (* used by get_unique_var *)

(* gets unique variable *)
let get_unique_var() =
    (c := !c+1; "___UNIQUE_VAR"^(string_of_int !c))

(* gets list of variables in term *)
let rec get_variables term list =
	let rec get_vars_from_args args list = (* gets variables from arguments *)
		match args with
			| [] -> list
			| t::terms -> get_vars_from_args terms (get_variables t list)
	in
		match term with
			| TermOr(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermAnd(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermVariable v -> if List.exists (fun var -> var = v) list then list else v::list
			| TermString str -> list
			| TermConstant const -> list
			| TermFunctor(nam,args) -> 
				get_vars_from_args args list
			| TermIs(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticPlus(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticMinus(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticMult(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticDiv(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticEquality(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticInequality(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticLess(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticGreater(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticLeq(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermArithmeticGeq(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermTermUnify(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermTermNotUnify(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermNegation t -> get_variables t list
			| TermTermEquality(t1,t2) -> get_variables t2 (get_variables t1 list)
			| TermList listterm ->
				(match listterm with
					| EmptyList -> list
					| NormalList args -> get_vars_from_args args list
					| DividedList (args,term) -> get_variables term (get_vars_from_args args list))
			| _ -> list

let map_uniques = List.map (fun var -> (var, TermVariable (get_unique_var ())))

(* makes variables in clause unique *)
let make_unique = function
	| SingleClause term -> 
		SingleClause (replace term 
						  (map_uniques (get_variables term [])))
	| ClauseImplication (term1, term2) ->
		let replacement = map_uniques (get_variables term2 (get_variables term1 [])) in
			ClauseImplication (replace term1 replacement,
							   replace term2 replacement)

(* evaluates arithmetic expression *)
let rec apply_arithmetic_operator t1 t2 f =
	let n1 = arithmetic_eval t1
	and n2 = arithmetic_eval t2
		in match n1, n2 with
			| Integer i1, Integer i2 -> Integer (f i1 i2)
and arithmetic_eval = function
	| TermConstant (ConstantNumber n) -> n
	| TermConstant _ -> raise Not_a_number
	| TermArithmeticPlus(t1,t2) -> apply_arithmetic_operator t1 t2 (+)
	| TermArithmeticMinus(t1,t2) -> apply_arithmetic_operator t1 t2 (-)
	| TermArithmeticMult(t1,t2) -> apply_arithmetic_operator t1 t2 ( * )
	| TermArithmeticDiv(t1,t2) -> apply_arithmetic_operator t1 t2 (/)
	| _ -> raise Not_a_number
			
(* evaluates functor 
functor_term is a term to evaluate
database is a database loaded into the program 
rep is a replacement
clauses is a list of clauses from database that haven't beed checked yet
cont is a continuation *)
let rec functor_eval functor_term database rep clauses sc fc cut_c =
	let term = replace functor_term rep in (* replace variables in term *)
		match clauses with
			| [] -> sc (false,[]) fc (* no more facts or implications in database *)
			| dclause::clauses'' ->
				let clauses' = Shuffle.shuffle clauses'' in
				let clause = make_unique dclause in
					(match clause with
						| SingleClause dterm -> 
							let uni = (unify term dterm rep)   (* found a fact in database *)
							in
								if fst uni 
								then sc uni (fun() -> functor_eval term database rep clauses' sc fc cut_c) (* term 
								   unifies with fact in database, so store rest of possible calculations 
								   and return result of unification *)
								else functor_eval term database rep clauses' sc fc cut_c (* term doesn't unify 
																	with fact, so try another possibilities *)
						| ClauseImplication (dterm, condition) ->
							let uni = (unify term dterm rep) (* found an implication in database, 
																try to unificate with it's resault (left side term) *)
							in		 
								if fst uni 
								then evaluate condition database (snd uni) database 
									(fun vt fc' -> sc vt fc') (fun () -> functor_eval term database rep clauses' sc fc cut_c) fc
								else functor_eval term database rep clauses' sc fc cut_c)
						
(* evaluates terms *)
and evaluate term database rep clauses sc fc cut_c =
	let arithmetic_comparison t1 t2 f =
		let n1 = arithmetic_eval t1
		and n2 = arithmetic_eval t2
		in
			(match n1, n2 with
				| Integer i1, Integer i2 -> sc ((f i1 i2), rep) fc)
	and arithmetic_equality t1 t2 flag =
		let n1 = arithmetic_eval t1
		and n2 = arithmetic_eval t2
		in
		    if (n1 = n2) = flag
			then sc (true, rep) fc
			else sc (false, []) fc
	in

	let repterm = replace term rep             (* apply replacement to the term *)
	in
		match repterm with
			| TermTermUnify(term1,term2) -> sc (unify term1 term2 rep) fc
			| TermTermNotUnify(term1,term2) -> 
				let uni = unify term1 term2 rep in
					sc (not (fst uni), snd uni) fc
			| TermArithmeticEquality(t1,t2) -> arithmetic_equality t1 t2 true
			| TermArithmeticInequality(t1,t2) -> arithmetic_equality t1 t2 false
			| TermArithmeticLess(t1,t2) -> arithmetic_comparison t1 t2 (<)
			| TermArithmeticGreater(t1,t2) -> arithmetic_comparison t1 t2 (>)
			| TermArithmeticLeq(t1,t2) -> arithmetic_comparison t1 t2 (<=)
			| TermArithmeticGeq(t1,t2) -> arithmetic_comparison t1 t2 (>=)
			| TermNegation t ->
				evaluate t database rep clauses
					(fun vt fc' -> sc (not (fst vt), snd vt) fc') fc cut_c
			| TermTermEquality(t1,t2) -> sc (t1 = t2,rep) fc
			| TermIs(t1,t2) -> 
				let n2 = TermConstant (ConstantNumber (arithmetic_eval t2))
				in
					sc (unify t1 n2 []) fc
			| TermFunctor(nam,args) -> functor_eval repterm database rep clauses sc fc cut_c
			| TermAnd(t1,t2) -> 
				evaluate t1 database rep clauses (* evaluate first term *)
					(fun vt1 fc1 ->
						if fst vt1 then
							evaluate t2 database (snd vt1) clauses
								(fun vt2 fc2 -> sc vt2 fc2) fc1 cut_c (* if first term returns true in evaluation 
																		 then the other one will be tried to be evaluated *)
						else sc (false,[]) fc1) fc cut_c
			| TermOr(t1,t2) -> evaluate t1 database rep clauses
				(fun vt fc' -> sc vt fc')
				(fun () -> evaluate t2 database rep clauses sc fc cut_c) cut_c (* evaluate first term *)
			| TermCut -> sc (true,rep) cut_c
			| _ -> raise Cant_evaluate

let quiet = false

let thunk = fun () -> ()

let print_no = fun () -> 
	if quiet
	then ()
	else print_string "No\n"

let print_yes = fun () ->
	if quiet
	then ()
	else print_string "Yes\n"

(* evaluates all possible ways a term given a specific database *)
let interpret term database interactive one_shot =
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
  and filter replacement = (* filters replacement that only variables that exist in term remains *)
    let variables = (get_variables term []) in
    List.filter (fun (var,_) -> List.exists (fun v -> v = var) variables) replacement
  in
      evaluate term database [] database
		  (fun vt fc -> 
			  if fst vt 
			  then (print_yes ();
					print_replacement (filter (snd vt)); 
					print_endline "";
					if more () 
					then fc () 
					else ()
			  )
			  else fc ()
		  )
		  print_no thunk
		  

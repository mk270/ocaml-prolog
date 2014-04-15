open Types

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

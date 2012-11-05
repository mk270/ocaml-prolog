open Types;;

exception Unsuported_term;;
exception Type_error;;

(* makes a string of term *)
let rec string_of_term term =
  let rec string_of_arguments args =
  match args with
	      [] -> ""
	    | [t] -> (string_of_term t)
	    | t::terms -> (string_of_term t)^","^(string_of_arguments terms)
  in
  match term with
      TermOr(t1,t2) -> (string_of_term t1)^"; "^(string_of_term t2)
    | TermAnd(t1,t2) -> (string_of_term t1)^", "^(string_of_term t2)
    | TermVariable v -> v
    | TermString str -> str
    | TermConstant const -> (match const with
				  ConstantAtom str -> str
				| ConstantNumber num -> (match num with
						| Integer n -> string_of_int n)
	)
    | TermFunctor(nam,args) ->
	
	  nam^"("^(string_of_arguments args)^")"
    | TermIs(t1,t2) -> (string_of_term t1)^" is "^(string_of_term t2)
    | TermIfThen(t1,t2) -> "if "^(string_of_term t1)^" then "^(string_of_term t2)
    | TermIfThenElse(t1,t2,t3) -> "if "^(string_of_term t1)^" then "^(string_of_term t2)^" else "^(string_of_term t3)
    | TermArithmeticPlus(t1,t2) -> (string_of_term t1)^" + "^(string_of_term t2)
    | TermArithmeticMinus(t1,t2) -> (string_of_term t1)^" - "^(string_of_term t2)
    | TermArithmeticMult(t1,t2) -> (string_of_term t1)^" * "^(string_of_term t2)
    | TermArithmeticDiv(t1,t2) -> (string_of_term t1)^" / "^(string_of_term t2)
    | TermArithmeticIntDiv(t1,t2) -> (string_of_term t1)^" // "^(string_of_term t2)
    | TermArithmeticModulo(t1,t2) -> (string_of_term t1)^" mod "^(string_of_term t2)
    | TermArithmeticEquality(t1,t2) -> (string_of_term t1)^" =:= "^(string_of_term t2)
    | TermArithmeticInequality(t1,t2) -> (string_of_term t1)^" =\\= "^(string_of_term t2)
    | TermArithmeticLess(t1,t2) -> (string_of_term t1)^" < "^(string_of_term t2)
    | TermArithmeticGreater(t1,t2) -> (string_of_term t1)^" > "^(string_of_term t2)
    | TermArithmeticLeq(t1,t2) -> (string_of_term t1)^" <= "^(string_of_term t2)
    | TermArithmeticGeq(t1,t2) -> (string_of_term t1)^" >= "^(string_of_term t2)
    | TermTermEquality(t1,t2) -> (string_of_term t1)^" == "^(string_of_term t2)
    | TermTermUnify(t1,t2) -> (string_of_term t1)^" = "^(string_of_term t2)
    | TermNegation t -> "not "^(string_of_term t)
    | TermTermNotUnify(t1,t2) -> (string_of_term t1)^" /= "^(string_of_term t2)
    | TermList list ->
	(match list with
	    EmptyList -> "[]"
	  | NormalList args -> "["^(string_of_arguments args)^"]"
	  | DividedList(args,term) -> "["^(string_of_arguments args)^" | "^(string_of_term term)^"]")
    | _ -> "";;



(* writes replacement to standard output *)
let rec print_replacement rep =
  match rep with
      [] -> ()
    | (v,rep_term)::trep -> print_string (v^" = "^(string_of_term rep_term)^"   "); print_replacement trep;;


(* appends a replacement to a term *)
let rec replace term replacement = 
  let rep term' =                       (* used for maping in TermFunctor *)
      replace term' replacement
  in
  match term with
      TermOr(t1,t2) -> TermOr(replace t1 replacement, replace t2 replacement)
    | TermAnd(t1,t2) -> TermAnd(replace t1 replacement, replace t2 replacement)
    | TermVariable var ->
	 (match replacement with
	     [] -> term
	   | (v,rep)::replacement' -> 
	       if v = var then rep   (* found current variable var in replacement *)
	       else replace term replacement') (* continues searching for variable *)
    | TermFunctor(nam,args) -> TermFunctor(nam,List.map rep args)
    | TermIs(t1,t2) -> TermIs(replace t1 replacement, replace t2 replacement)
    | TermArithmeticPlus(t1,t2) -> TermArithmeticPlus(replace t1 replacement, replace t2 replacement)
    | TermArithmeticMinus(t1,t2) -> TermArithmeticMinus(replace t1 replacement, replace t2 replacement)
    | TermArithmeticMult(t1,t2) -> TermArithmeticMult(replace t1 replacement, replace t2 replacement)
    | TermArithmeticDiv(t1,t2) -> TermArithmeticDiv(replace t1 replacement, replace t2 replacement)
    | TermArithmeticIntDiv(t1,t2) -> TermArithmeticIntDiv(replace t1 replacement, replace t2 replacement)
    | TermArithmeticModulo(t1,t2) -> TermArithmeticModulo(replace t1 replacement, replace t2 replacement)
    | TermArithmeticEquality(t1,t2) -> TermArithmeticEquality(replace t1 replacement, replace t2 replacement)
    | TermArithmeticInequality(t1,t2) -> TermArithmeticInequality(replace t1 replacement, replace t2 replacement)
    | TermArithmeticLess(t1,t2) -> TermArithmeticLess(replace t1 replacement, replace t2 replacement)
    | TermArithmeticGreater(t1,t2) -> TermArithmeticGreater(replace t1 replacement, replace t2 replacement)
    | TermArithmeticLeq(t1,t2) -> TermArithmeticLeq(replace t1 replacement, replace t2 replacement)
    | TermArithmeticGeq(t1,t2) -> TermArithmeticGeq(replace t1 replacement, replace t2 replacement)
    | TermTermEquality(t1,t2) -> TermTermEquality(replace t1 replacement, replace t2 replacement)
    | TermTermUnify(t1,t2) -> TermTermUnify(replace t1 replacement, replace t2 replacement)
    | TermTermNotUnify(t1,t2) -> TermTermNotUnify(replace t1 replacement, replace t2 replacement)
    | TermNegation t -> TermNegation (replace t replacement)
    | TermList list ->
	(match list with
	    EmptyList -> term
	  | NormalList args -> TermList (NormalList (List.map rep args))
	  | DividedList(args,term) -> TermList (DividedList(List.map rep args, rep term)))
    | _ -> term


(* adds new variable replacement to given replacement *)
let rec add_replacement (var,term) replacement =
  let replace_rep (var',term') =
    (var',replace term' [(var,term)])   (* replaces variable var if it is in the replacement *)
  in
    (var,term)::(List.map replace_rep replacement) (* replaces var in every node of replacement *)
    

(* tries to unify two terms, returns if terms can be unified and replacement needed for unification *)
let rec unify term1 term2 rep =
  let rec unify_args args1 args2 rep = (* unifies arguments of functors *)
    (match args1 with
	 [] -> (true,rep)
       | term1::terms1 -> (match args2 with
			       [] -> raise Type_error
			     | term2::terms2 -> let uni = unify term1 term2 rep
			       in
				 if fst uni then unify_args terms1 terms2 (snd uni) else (false,[])))
  and divide_list list n =
    let rec divlist list1 list2 n =
      match list2 with
	  [] -> (list1,[])
	| hd::tl -> if n = 0 then (list1,list2) else divlist (hd::list1) tl (n-1)
    in
    let (list1,list2) = divlist [] list n
    in
      (List.rev list1, list2)  
  and rterm1 = replace term1 rep  (* append replacment for terms to unify *)
  and rterm2 = replace term2 rep
  
  in
    if rterm1 = rterm2 then (true,rep)  (* terms are the same *)
    else
      match rterm1 with
	  TermVariable v1 -> (true,(add_replacement (v1,rterm2) rep)) (* left term is a variable so we add this variable to replacement (it will be replaced by right term *)
	| _ ->  (* left term is not a variable *)
	    (match rterm2 with
		 TermAnd(t21,t22) -> (match rterm1 with
					 TermAnd(t11,t12) -> let uni = unify t11 t21 rep in
					   if fst uni then unify t12 t22 (snd uni)
					   else (false,[])
				       | _ -> (false,[]))
	       | TermOr(t21,t22) -> (match rterm1 with
					 TermOr(t11,t12) -> let uni = unify t11 t21 rep in
					   if fst uni then unify t12 t22 (snd uni)
					   else (false,[])
				       | _ -> (false,[]))
	       | TermVariable v2 -> (true,(add_replacement (v2,rterm1) rep))
	       | TermIs(t21,t22) -> (match rterm1 with
					 TermIs(t11,t12) -> let uni = unify t11 t21 rep in
					   if fst uni then unify t12 t22 (snd uni)
					   else (false,[])
				       | _ -> (false,[]))
	       | TermArithmeticPlus(t21,t22) -> (match rterm1 with
						     TermArithmeticPlus(t11,t12) -> let uni = unify t11 t21 rep in
						       if fst uni then unify t12 t22 (snd uni)
						       else (false,[])
						   | _ -> (false,[]))
						  
	       | TermArithmeticMinus(t21,t22) -> (match rterm1 with
						      TermArithmeticMinus(t11,t12) -> let uni = unify t11 t21 rep in
							if fst uni then unify t12 t22 (snd uni)
							else (false,[])
						    | _ -> (false,[]))
	       | TermArithmeticMult(t21,t22) -> (match rterm1 with
						     TermArithmeticMult(t11,t12) -> let uni = unify t11 t21 rep in
						       if fst uni then unify t12 t22 (snd uni)
						       else (false,[])
						   | _ -> (false,[]))
	       | TermArithmeticDiv(t21,t22) -> (match rterm1 with
						    TermArithmeticDiv(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticIntDiv(t21,t22) -> (match rterm1 with
						    TermArithmeticIntDiv(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticModulo(t21,t22) -> (match rterm1 with
						    TermArithmeticModulo(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticEquality(t21,t22) -> (match rterm1 with
						    TermArithmeticEquality(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticInequality(t21,t22) -> (match rterm1 with
						    TermArithmeticInequality(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticLess(t21,t22) -> (match rterm1 with
						    TermArithmeticLess(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticGreater(t21,t22) -> (match rterm1 with
						    TermArithmeticGreater(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticLeq(t21,t22) -> (match rterm1 with
						    TermArithmeticLeq(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermArithmeticGeq(t21,t22) -> (match rterm1 with
						    TermArithmeticGeq(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermTermEquality(t21,t22) -> (match rterm1 with
						   TermTermEquality(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						 | _ -> (false,[]))
	       | TermTermUnify(t21,t22) -> (match rterm1 with
						TermTermUnify(t11,t12) -> let uni1 = unify t11 t21 rep in
						  if fst uni1 then unify t12 t22 (snd uni1)
						  else (false,[])
					      | _ -> (false,[]))
	       | TermTermNotUnify(t21,t22) -> (match rterm1 with
						    TermTermNotUnify(t11,t12) -> let uni1 = unify t11 t21 rep in
						      if fst uni1 then unify t12 t22 (snd uni1)
						      else (false,[])
						  | _ -> (false,[]))
	       | TermNegation t2 -> (match rterm1 with
						    TermNegation t1 -> unify t1 t2 rep
						  | _ -> (false,[]))
	       | TermFunctor(nam2,args2) -> 		   
		     (match rterm1 with
			  TermFunctor(nam1,args1) ->
			    if nam1 = nam2 && (List.length args1) = (List.length args2) then
			      unify_args args1 args2 rep
			    else (false,[])
			| _ -> (false,[]))
	       | TermList (EmptyList) -> (match rterm1 with
					      TermList (EmptyList) -> (true,rep)
					    | TermList (NormalList []) -> (true,rep)
					    | _ ->  (false,[]))
	       | TermList (NormalList args2) -> (match rterm1 with
						     TermList (EmptyList) ->
						       if (List.length args2) = 0 then
							 (true,rep)
						       else (false,[])
						   | TermList (NormalList args1) ->
						       if (List.length args2) = (List.length args1) then
							 unify_args args1 args2 rep
						       else (false,[])
						   | TermList (DividedList (args1,term)) ->
						       if (List.length args1) > (List.length args2) then
							 (false,[])
						       else
							 let (args2',args2'') = divide_list args2 (List.length args1)
							 in
							 let uni = unify_args args2' args1 rep in
							   if fst uni then
							     unify (TermList (NormalList args2'')) term (snd uni)
							   else
							     (false,[])
						   | _ -> (false,[]))
	       | TermList (DividedList(args2,term2)) -> (match rterm1 with
							   TermList (NormalList args1) ->
							     if (List.length args2) > (List.length args1) then
							       (false,[])
							     else
							       let (args1',args1'') = divide_list args1 (List.length args2)
							       in
							       let uni = unify_args args2 args1' rep in
								 if fst uni then
								   unify (TermList (NormalList args1'')) term2 (snd uni)
								 else
								   (false,[])
							 | TermList (DividedList(args1,term1)) ->
							     if (List.length args2) >= (List.length args1) then
							       let (args2', args2'') = divide_list args2 (List.length args1)
							       in
							       let uni = unify_args args2' args1 rep in
								 if fst uni then
								   if (List.length args2'') = 0 then
								     unify term1 term2 (snd uni)
								   else
								     unify term1 (TermList (DividedList (args2'',term2))) (snd uni)
								 else
								   (false,[])
							     else
							       let (args1', args1'') = divide_list args1 (List.length args2)
							       in
							       let uni = unify_args args2 args1' rep in
								 if fst uni then
								   unify (TermList (DividedList (args1'',term1))) term2 (snd uni)
								 else 
								   (false,[])
							 | _ -> (false,[]))
		   
							 
	       | _ -> if rterm1 = rterm2 then (true,rep) else (false,[]))







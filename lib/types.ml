(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

(* --- Prolog syntax elements constructors --- *)


type name = string       (* prolog names *)

type number =
  | Integer of int      (* prolog integers *)

type constant =  
  | ConstantAtom of name                 (* constants: atom *)
  | ConstantNumber of number             (* constant: number *)

type binary_operator =
  | TermOr                                      (* logical disjunction *)
  | TermAnd                                     (* logical conjuction *)
  | TermIs                                      (* term1 is term2 *)
  | TermArithmeticPlus                          (* term1 + term2 *)            
  | TermArithmeticMinus                         (* term1 - term2 *)       
  | TermArithmeticMult                          (* term1 * term2 *)      
  | TermArithmeticDiv                           (* term1 / term2 *)    
  | TermArithmeticEquality                      (* term1 =:= term2 *)          
  | TermArithmeticInequality                    (* term1 =\= term2 *)            
  | TermArithmeticLess                          (* term1 < term2 *)      
  | TermArithmeticGreater                       (* term1 > term2 *)         
  | TermArithmeticLeq                           (* term1 <= term2 *)     
  | TermArithmeticGeq                           (* term1 >= term2 *)     
  | TermTermEquality                            (* term1 == term2 *)    
  | TermTermUnify                               (* term1 = term2 *) 
  | TermTermNotUnify                            (* term1 \= term2 *)

type term =                                     (* prolog term types *)
  | TermBinOp of binary_operator * term * term  (* binary operation *)
  | TermString of string                        (* prolog string term *)
  | TermConstant of constant                    (* prolog constant term *)
  | TermVariable of name                        (* prolog variable terms *)
  | TermFunctor of name * arguments             (* functor term *)
  | TermList of lists                           (* prolog lists *)

  | TermIfThen of term * term                   (* if then *)
  | TermIfThenElse of term * term * term        (* if then else *)

  | TermNegation of term                        (* not *)

  | TermDecomposition of term * term            (* term1 =.. term2 *)
  | TermTermInequality of term * term           (* term \== term2 *)
  | TermTermOrderEquality of term * term        (* term1 @= term2 *)
  | TermTermOrderInequality of term * term      (* term1 @\= term2 *)
  | TermTermOrderLess of term * term            (* term1 @< term2 *)
  | TermTermOrderGreater of term * term         (* term1 @> term2 *)
  | TermTermOrderGeq of term * term             (* term1 @>= term2 *)
  | TermTermOrderLeq of term * term             (* term1 @=< term2 *)

  | TermModule of term * term                   (* term1 : term2 *)

  | TermVariableInstantiated of term            (* ^term1 *)
  | TermBitwiseAnd of term * term               (* /\ *)
  | TermBitwiseOr of term * term                (* \/ *)
  | TermBitwiseNot of term * term               (* \ *)

  | TermCut                                     (* ! *)

and lists =
  | EmptyList
  | NormalList of arguments
  | DividedList of arguments * term

and arguments = term list                (* functor arguments *)

type clause =                                   (* database clauses *)
  | ClauseImplication of term * term            (* implication clause *)
  | SingleClause of term                        (* fact clause *)

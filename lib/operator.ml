
open Types

exception Invalid_arithmetic_operator

let string_of_binary_op = function
	| TermIs -> " is "
	| TermOr -> "; "
	| TermAnd -> ", "
	| TermArithmeticPlus -> " + "
	| TermArithmeticMinus -> " - "
	| TermArithmeticMult -> " * " 
	| TermArithmeticDiv -> " / " 
	| TermArithmeticEquality -> " =:= "
	| TermArithmeticInequality -> " =\\= "
	| TermArithmeticLess -> " < "
	| TermArithmeticGreater -> " > "
	| TermArithmeticLeq -> " <= "
	| TermArithmeticGeq -> " >= "
	| TermTermEquality -> " == " 
	| TermTermUnify -> " = "
	| TermTermNotUnify -> " /= "

let function_of_operation = function
	| TermArithmeticPlus -> (+)
	| TermArithmeticMinus -> (-)
	| TermArithmeticMult -> ( * )
	| TermArithmeticDiv -> (/)
	| _ -> raise Invalid_arithmetic_operator

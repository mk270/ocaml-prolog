
type interp_behaviour = Evaluator.interp_behaviour

let clauses_from_string s =
	Lexing.from_string s |> Parser.clause_list Lexer.token 

let term_from_string s =
	Lexing.from_string s |> Parser.query Lexer.token

let interpret = Evaluator.interpret


let clauses_from_string s =
	Lexing.from_string s |> Parser.clause_list Lexer.token 

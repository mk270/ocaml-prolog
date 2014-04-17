
open Types

type interp_behaviour = Evaluator.interp_behaviour

val clauses_from_string : string -> clause list
val term_from_string : string -> term
val interpret : term -> clause list -> Evaluator.interp_behaviour -> unit

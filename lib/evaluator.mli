
open Types

val get_variables : term -> name list -> name list
val maybe_shuffle : bool -> 'a list -> 'a list

val evaluate : term -> clause list ->
  (name * term) list -> clause list ->
  (bool * (name * term) list ->  (unit -> 'result) -> 'result) ->
  (unit -> 'result) -> (unit -> 'result) -> bool -> 'result

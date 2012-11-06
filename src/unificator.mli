open Types

val string_of_term : term -> string
val print_replacement : (name * term) list -> unit
val replace : term -> (name * term) list -> term
val unify : term -> term -> (name * term) list -> bool * ((name * term) list)

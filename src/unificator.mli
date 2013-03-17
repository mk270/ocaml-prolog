(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Types

val string_of_term : term -> string
val print_replacement : (name * term) list -> unit
val replace : term -> (name * term) list -> term
val unify : term -> term -> (name * term) list -> bool * ((name * term) list)

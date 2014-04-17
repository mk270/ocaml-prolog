(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)
open Types

type interp_behaviour = {
	randomise : bool;
	interactive : bool;
	quiet : bool;
	limit : int option;
}

type database

val interpret : database -> interp_behaviour -> term -> unit
val clauses_from_string : string -> database
val term_from_string : string -> term

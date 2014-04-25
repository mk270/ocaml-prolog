
open Types

exception Invalid_arithmetic_operator

val string_of_binary_op : binary_operator -> string
val function_of_operation : binary_operator -> int -> int -> int

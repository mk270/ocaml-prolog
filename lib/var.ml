let c = ref 0 (* used by get_unique_var *)

let get_unique () =
  c := !c+1; 
  "_Var" ^ (string_of_int !c)

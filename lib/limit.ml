
let get_limit = function
  | None -> 1
  | Some n -> n

let set_limit = function
  | 0 -> None
  | n -> Some n

let bool_of = function
  | None -> false
  | Some _ -> true

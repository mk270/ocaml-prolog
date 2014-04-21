
let get_limit = function
	| None -> 1
	| Some n -> n

let set_limit = function
	| 0 -> None
	| n -> Some n

(*
  OCaml-Prolog, a Prolog interpreter, by Karol Stosiek and Szymon Fogiel

  Copyright (C) 2008  Karol Stosiek and Szymon Fogiel

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

let swap a i j =
    let t = a.(i) in
		a.(i) <- a.(j);
		a.(j) <- t

let shuffle_array a = 
	Array.iteri (fun i _ -> swap a i (Random.int (i+1))) a

let shuffle_list l =
	let tmp_array = Array.of_list l in
		shuffle_array tmp_array;
		Array.to_list tmp_array

let shuffle = shuffle_list

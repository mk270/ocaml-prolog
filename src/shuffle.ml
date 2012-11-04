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

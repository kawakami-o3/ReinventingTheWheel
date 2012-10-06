(* length : 'a list -> int *)
let rec length lst = match lst with
  [] -> 0
  | fisrt :: rest -> 1 + (length rest)

let length_test1 = length [] = 0
let length_test2 = length [1] = 1
let length_test3 = length [3; 4; 5] = 3


let rec nth lst n =
  if n<0 then failwith "nth" else
    match lst with
    [] -> failwith "nth"
    | first :: rest -> if n=0 then first else nth rest (n-1)

let nth_test1 = nth [2; 1] 0 = 2
let nth_test2 = nth [2; 1] 1 = 1


let rec append a b = match a with
  [] -> b
  | first :: rest -> first :: (append rest b)

let append_test1 = append [0] [1] = [0; 1]



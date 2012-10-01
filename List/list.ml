(* length : 'a list -> int *)
let rec length lst = match lst with
  [] -> 0
  | fisrt :: rest -> 1 + (length rest)

let length_test1 = length [] = 0
let length_test2 = length [1] = 1
let length_test3 = length [3; 4; 5] = 3


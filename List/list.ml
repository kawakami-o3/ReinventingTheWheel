
(* assertTrue : string -> bool list -> unit *)
let rec assertTrue str lst = match lst with
  [] -> print_string (str ^ " OK.\n")
  | first :: rest -> if first
                      then assertTrue str rest
                      else print_string (str ^ " NG!!!\n")

(* length : 'a list -> int *)
let rec length lst = match lst with
  [] -> 0
  | fisrt :: rest -> 1 + (length rest)


let length_test () = assertTrue "length" [length [] = 0; length [1] = 1; length [3; 4; 5] = 3]


let rec nth lst n =
  if n<0 then failwith "nth" else
    match lst with
    [] -> failwith "nth"
    | first :: rest -> if n=0 then first else nth rest (n-1)

let nth_test () = assertTrue "nth" [nth [2; 1] 0 = 2; nth [2; 1] 1 = 1]


let rec append a b = match a with
  [] -> b
  | first :: rest -> first :: (append rest b)

let append_test1 = append [0] [1] = [0; 1]


let rec concat lst = match lst with
  [] -> []
  | [] :: rest -> concat rest
  | (x::xs) :: rest -> x :: concat (xs::rest)

let concat_test () = assertTrue "concat"
  [ [1; 2; 3] = (concat [[1; 2]; [3]; []]);
    [1; 2; 3] = (concat [[1]; [2; 3]; []]);
    [1; 2; 3] = (concat [[]; [1]; [2; 3]]);
    [] = (concat []);
    [] = (concat [[]])
  ]


let rec map f lst = match lst with
  [] -> []
  | first :: rest -> f first :: map f rest

let map_test () = assertTrue "map"
  [ [2;3;4] = (map (fun x -> x + 1) [1;2;3]);
    [] = (map (fun x -> x + 1) [])
  ]


let rec concatMap f lst = match lst with
  [] -> []
  | first :: rest -> append (f first) (concatMap f rest)

let concatMap_test () = assertTrue "concatMap"
[ [1;-1;2;-2] = (concatMap (fun x -> [x; -x]) [1;2]);
    [] = (concatMap (fun x -> [x; -x]) [])
  ]


let rec filter f x = match x with
    [] -> []
  | h::t -> if f h then h :: filter f t else filter f t

let filter_test () =
  let even x = (x mod 2 = 0)
  in
  assertTrue "filter"
  [  [2;4] = (filter even [1;2;3;4]);
   [] = (filter even [1;3;5]);
   [] = (filter even [])
  ]


(* main : unit -> unit *)
let main () = (
  (*length_test ();*)
  (*nth_test ();*)
  (*concat_test ();*)
  (*map_test ()*)
  (*concatMap_test ()*)
  filter_test ()
  )

let _ = main ()

fun even n = n mod 2 = 0

fun assertTrue str lst =
  case lst of
       nil => print ("OK: "^str^"\n")
     | (h::t) => if h
                 then assertTrue str t
                 else print ("NG: "^str^"\n")


fun length lst = case lst of nil => 0
                       | (h::t) => 1 + length t

fun length_test () = [length [] = 0,
  length [1] = 1,
  length [1,2] = 2]


fun nth lst n =
  if n < 0 then raise Subscript else
  case lst of
       nil => raise Subscript
     | (h::t) => if n=0 then h else nth t (n-1)

fun nth_test () = [nth [2,1] 0 = 2, nth [2,1] 1 = 1]


fun append a b =
  case a of
       nil => b
     | (h::t) => h :: (append t b)

fun append_test () = [append [1] [2] = [1,2],
  append [] [2] = [2],
  append [1,2,3] [] = [1,2,3]]


(* concat :: a list list -> a list *)
fun concat lst =
  case lst of
       nil => nil
     | (nil::xs) => concat xs
     | ((y::ys)::xs) => y :: concat (ys::xs)

fun concat_test () =
  [ [1,2,3] = (concat [[1,2],[3],[]])
  , [1,2,3] = (concat [[1],[2,3],[]])
  , [1,2,3] = (concat [[],[1],[2,3]])
  , [] = (concat [])
  , [] = (concat [[]])
  ]


fun map f lst =
  case lst of
       [] => []
     | (h::t) => f h :: map f t

fun map_test () =
  [ [2,3,4] = (map (fn x => x + 1) [1,2,3])
    , [] = (map (fn x => x + 1) [])
  ]


fun concatMap f lst =
  case lst of
       [] => []
     | (x::xs) => append (f x) (concatMap f xs)

fun concatMap_test () =
  [ [1,~1,2,~2] = (concatMap (fn x => [x, ~x]) [1,2]),
    [] = (concatMap (fn x => [x, ~x]) [])
  ]


fun filter f lst =
  case lst of
       [] => []
     | (h::t) => if f h then h :: filter f t else filter f t

fun filter_test () =
  [ [2,4] = (filter even [1,2,3,4]),
    [] = (filter even [1,3,5]),
(*      ([] : int list) = (filter even [])*)
    ([]) = (filter even [])
  ] 

(*
fun until f g x = x

fun until_test () =
  [ 0.78125 = (until (<=
  ]
  *)


(*---------------------------------------------------------*)
fun main () = (
  (* assertTrue "length" (length_test ()) *)
  (* assertTrue "nth" (nth_test ()) *)
  (*assertTrue "append" (append_test ())*)
  (*assertTrue "concat" (concat_test ())*)
  (*assertTrue "map" (map_test ())*)
  (*assertTrue "concatMap" (concatMap_test ())*)
  assertTrue "filter" (filter_test ())
  (*assertTrue "until" (until_test ())*)
);

main ();


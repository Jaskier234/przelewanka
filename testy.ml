open Przelewanka;;

Random.init 43;;

let a = [| (2,1); (3,3) |];;
assert( przelewanka a = 4 );;

let a = [| (2,1); (3,0) |];;
assert( przelewanka a = 4);;

let a = [| |];;
assert( przelewanka a = 0);;

let a = Array.init 7 (fun _ ->let x = Random.int 5 in (x,x));;
assert( przelewanka a = 5);;

let a = [| (2,1); (3,1); |];;
assert( przelewanka a = -1);;

let a = [| (6,3); (10,0); (15,7) |];;
assert( przelewanka a = 10);;

let a = [| (6,6); (10,10); (15,15) |];;
assert( przelewanka a = 3);;

(* większe *)

let a = Array.make 10 (1,1);;
assert( przelewanka a = 10);;

let a = [|(2, 1); (2, 2); (2, 0); (2, 0); (2, 1); (2, 2); (2, 1); (2, 2); (2, 2); (2, 0)|];;
assert( przelewanka a = -1);;

let a = [|(1, 1); (2, 2); (2, 0); (2, 0); (2, 1); (2, 2); (2, 1); (2, 2); (2, 2); (2, 0)|];;
assert( przelewanka a = 8);;

(* warunek nwd *)

let a = [| (4,3); (2,2) |];;
assert( przelewanka a = -1);;

let a = [| (5,3); (10,0); (15,7) |];;
assert( przelewanka a = -1);;

let a = [| (648,582); (435,0); (234,174) |];;
assert( przelewanka a = 26);;

(* conajmniej 1 pusty lub pełny *)

let a = Array.make 100000 (43,12);;
assert( przelewanka a = -1);;

(* zera *)

let a = [| (0, 0); (7,5); (5,0); (3,2); (0,0); (0,0) |];;
assert( przelewanka a = 4);;

let a = Array.make 100000 (0, 0);;
assert( przelewanka a = 0);;

a.(0) <- (49, 7);
a.(1) <- (21, 21);
assert( przelewanka a = 4);;

(* duże *)

let a = [| (100000, 99999); (1,0) |];;
assert( przelewanka a = 3 )

let a = [| (100000,50647); (99999,0) |];;
let x = przelewanka a;;
assert( x > 0 );;
assert( x = 197410 );;

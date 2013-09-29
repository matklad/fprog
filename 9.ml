open Core.Std;;


let identity n =
  let f i = List.init n ~f:(fun j -> if i = j then 1 else 0) in
  List.init n ~f
;;

assert(identity 4 = [[1; 0; 0; 0];
                     [0; 1; 0; 0];
                     [0; 0; 1; 0];
                     [0; 0; 0; 1]])
;;

let odd x = x mod 2 = 1
;;
let countOdd =
  let f x acc = acc + (if odd x then 1 else 0) in
  List.fold_right ~f ~init:0
;;
let countOdd1 = List.count ~f:odd
;;
assert(countOdd  [2; 3; 4; 8; 5; 33; 9] = 4);
assert(countOdd1 [2; 3; 4; 8; 5; 33; 9] = 4)
;;
let myfoldl f init ls =
  let g x fn y = f y x |> fn in
  init |> List.fold_right ~f:g ~init:Fn.id ls
;;
assert(myfoldl ((+)) 0 [1; 2; 3] = 6);
assert(myfoldl ((^)) "" ["a"; "b"; "c"] = "abc");
assert(myfoldl ((/.)) 1.0 [1.0; 2.0; 3.0] = ((1.0/.1.0)/.2.0)/.3.0)
;;
let repeatFunc f n =
  let fs = List.init n (Fn.const f) in
  List.fold_left ~init:Fn.id ~f:Fn.compose fs
;;
let f = repeatFunc ((+) 3) 3 in
assert(f 1 = 10)
;;

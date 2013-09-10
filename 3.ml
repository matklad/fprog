let rec minlist = function
  | []    -> failwith "empty List"
  | [x]   -> x
  | x::xs -> min x (minlist xs)
;;
assert(minlist [3; 2; 7;] = 2);
assert(minlist [1]    = 1);
assert(minlist [1; 2] = 1);
assert(minlist [2; 1] = 1);;

let minsum xs =
  let rec sums = function
    | x::y::xs -> (x+y)::sums (y::xs)
    | _ -> []
  in
  minlist (sums xs)
;;
assert(minsum [1; 8; 3; 2; 7] = 5);
assert(minsum [1; 2] = 3);
assert(minsum [1; 2; 0] = 2)

let rev =
  let rec go acc = function
    | [] -> acc
    | x::xs -> go (x::acc) xs
  in go []
;;
assert(rev [1;3;7] = [7;3;1])

let rec check cond = function
  | [] -> false
  | x::xs -> cond x || check cond xs
;;
assert(check (fun x -> x > 5) [3; 2; 7; 4] = true);
assert(check (fun x -> x < 1) [3; 2; 7; 4] = false)

let rec checkDifferent = function
  | [] -> true
  | x::xs -> not(check ((=)x) xs) && checkDifferent xs
;;
assert(checkDifferent  [3; 2; 7] = true);
assert(checkDifferent  [3; 2; 7; 5; 7; 8] = false)

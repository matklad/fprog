module L = List;;
module A = Array;;

let rec gcd a b = if a = 0 then b else gcd (b mod a) a

let parts xs = let
  aux acc x = match acc with
  | ((a::first)::rest)-> if x > a
    then (x::a::first)::rest
    else [x]::(a::first)::rest
  | _  -> [[x]]
  in let split xs = L.fold_left aux [] xs
  in L.fold_left gcd 0 (L.map L.length @@ split xs) > 1
;;

assert(parts [1;2;8;2;5;6]);
assert(parts [1; 2; 3; 4; 5]);
assert(parts [1; 2; 1; 2; 3; 4]);
assert(not @@ parts [4; 6; 3; 5; 7]);
assert(not @@ parts []);
assert(not @@ parts [4]);
assert(parts [2;4])
;;

let any f xs = L.fold_left (||) false (L.map f xs);;
let null = function | [] -> true | _ -> false;;
let nub = L.fold_left (fun acc el -> if L.mem el acc then acc else el::acc) [];;
let count ls el = L.length @@ L.filter ((=) el) ls

(*
|    1, 3, 1, 8, 4, 9
V    0  1  2  3  4  5

    0     2   3     4
    * --- *   * --- *
         /
       1/     5
       *      *

   0  1  2  3  4  5
   x  x !x  y !y  z

    (1, 0)
    (1, 1)
    (2, 1) <-->
sum (4, 2) =(

    (1, 0)
    (1, 1)
    (1, 2)
sum (3, 3) =)
*)

let mk_graph ls = let
  n = L.length ls
  in let rec constraints i =
    let x = L.nth ls i
    in L.concat (L.mapi (fun j el -> if (i < j && x >= el) || (i > j && x <= el)
                                then [j]
                                else []) ls)
  in A.init n constraints
;;

exception CycleConstraint
let mk_components ls graph = let
  n = L.length ls
  in let values = A.make n 0
  in let rec dfs m v =
    if values.(v) = m
    then ()
    else if values.(v) = -m
    then raise CycleConstraint
    else if values.(v) = 0
    then
      begin
        values.(v) <- m;
        L.iter (fun el -> dfs ~-m el) graph.(v)
      end
    else failwith "impossible happend!"
  in
  A.iteri (fun i el -> if el != 0 then () else dfs (i+1) i) values;
  A.to_list values
;;

let condense ls = A.of_list @@ L.map
    (fun x -> (count ls x, count ls (~-x)))
    (nub @@ L.map abs ls)
;;

let parts2 ls = let n = L.length ls
  in
  if n mod 2 = 1
  then false
  else
    let graph = mk_graph ls
    in
    try
      let components = mk_components ls graph
      in let condensed = condense components
      in let h = Hashtbl.create (n * n);
      (*
         0 <= i <= n, 0 <= r <= n, 0 <= l <= n
         so it looks like n^^3 but it's n ^^ 2
         because for each i (l + r) is fixed
      *)
      in let rec f i l r = if i = A.length condensed
        then if l = 0 && r = 0
          then true
          else false
        else let key = (i, l, r)
          in
          begin
            if Hashtbl.mem h key
            then ()
            else
              let (a, b) = condensed.(i)
              in let value = f (succ i) (l - a) (r - b)
                             || f (succ i) (l - b) (r - a)
              in
              Hashtbl.add h key value
          end;
          Hashtbl.find h key
      in f 0 (n / 2) (n / 2)
    with CycleConstraint -> false
;;

assert (parts2 [1; 3; 1; 8; 4; 9] = true);;
assert (parts2 [1; 2; 3; 5; 1; 7] = false);;

let parts2_bf ls = let rec
  partition = function
  | [] -> [([], [])]
  | (x::xs) -> let
    aux (ls, rs) = (if null ls || L.hd ls > x then [(x::ls, rs)] else [])
                   @ (if null rs || L.hd rs > x then [(ls, x::rs)] else [])
    in L.concat (L.map aux @@ partition xs)
  in any (fun (a, b) -> L.length a = L.length b) @@ partition ls
;;

let rec r_list n = if n = 0 then [] else (Random.int 9000)::r_list (n-1);;

Random.self_init ();;

let rec desc n = if n = 0 then [] else n::desc (n-1);;
for i = 1 to 10000 do
  let l = r_list 8
  in
  assert (parts2_bf l = parts2 l)
done

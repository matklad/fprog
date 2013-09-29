open Core.Std;;

let powerset ls =
  let f acc x = acc @ List.map acc ~f:(fun xs -> x::xs) in
  List.fold_left ~f ~init:[[]] ls
;;
assert(powerset [1; 2; 3] = [[]; [1]; [2]; [2;1]; [3]; [3;1]; [3;2]; [3;2;1]])
;;

let myreverse ls =
  [] |> List.fold_right ~f:(fun x fn y -> x::y |> fn) ~init:Fn.id ls
;;
assert(myreverse [1;2;3] = [3;2;1]);
assert(myreverse [1] = [1]);
assert(myreverse [] = [])
;;

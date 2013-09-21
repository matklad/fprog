open Core.Std;;


let isosc (x1, y1) (x2, y2) (x3, y3) =
  let hypot a b = a*a + b*b in
  let a = hypot (x1 - x2) (y1 - y2) in
  let b = hypot (x2 - x3) (y2 - y3) in
  let c = hypot (x3 - x1) (y3 - y1) in
  a = b || b = c || c = a
;;
assert(isosc (0, 2) (2, 0) (2,2));
assert(not @@ isosc (-3, 2) (2, 0) (2,2));;

let cubeTable n = List.init n (fun i -> let x = succ i in (x, x*x*x))
;;
assert(cubeTable 4 =  [(1, 1); (2, 8); (3, 27); (4, 64)]);;

let minsum ls =
  let init = List.take ls @@ List.length ls - 1 in
  let tail = List.tl_exn ls in
  List.map2_exn init tail ~f:(+) |> List.reduce_exn ~f:min
;;
assert(minsum [1; 8; 3; 2; 7] = 5);;


type 'a tree = Empty | Node of 'a * 'a tree * 'a tree
;;

let rec height = function
    | Empty -> -1
    | (Node (_, left, right)) -> 1 + max (height left) (height right)
;;
assert(height (Node (1, Empty, Empty)) = 0);
assert(height (Node (1, (Node (1, Empty, Empty)), Empty)) = 1);;

type 'a queue = Queue of 'a list * 'a list;;

let enqueue el (Queue (e, d)) = Queue (el::e, d)
;;

let rec dequeue (Queue (e, d)) = match (e, d) with
  |([], []) -> failwith "dequeue: empty queue"
  |(e, d::ds) -> (d, Queue (e, ds))
  |(e, []) -> dequeue (Queue ([], List.rev e))
;;

let empty_queue = Queue ([], [])
;;

let min_height tree =
  let rec bfs q =
    let ((t, h), q') = dequeue q in
    match t with
    | Empty -> h
    | (Node (_, left, right)) ->
      enqueue (left, succ h) q' |> enqueue (right, succ h) |> bfs
  in
  enqueue (tree, -1) empty_queue |> bfs
;;

assert(min_height (Node (1, Empty, Empty)) = 0);
let t = (Node (1,
               (Node (2,
                      (Node (3, Empty, Empty)),
                      Empty)),
               (Node (4, Empty, Empty))))
in assert(min_height t = 1);;

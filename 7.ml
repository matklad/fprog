let rec egcd a b = if b = 0
  then (a, 1, 0)
  else let (g, x, y) = egcd b (a mod b) in
    (g, y, x - y * ( a / b))
;;

let euclid a b = let (g, x, y) = egcd a b in
  assert(g = 1);
  (x, y)
;;
assert(euclid 3 5 = (2, -1))
;;

let gcd a b = let (g, _, _) = egcd a b in g
;;

type equation = {var1: int;
                 var2: int;
                 a: int;
                 b: int;
                 d: int}

let print_eq eq =
    Printf.printf "V%d*%d + V%d*%d = %d;\n" eq.var1 eq.var2 eq.a eq.b eq.d
;;

let substitite old_var (new_var, vk, v0) eq =
  if old_var = eq.var1
  then {var1 = new_var; var2 = eq.var2;
        a = eq.a * vk; b = eq.b; d = eq.d - eq.a * v0}
  else
  if old_var = eq.var2
  then {var1 = eq.var1; var2 = new_var;
        a = eq.a; b = eq.b * vk; d = eq.d - eq.b * v0}
  else eq
;;

type solution = Any | Line of (int*int) * (int*int)

let solve_d a b d =
  let (g, x, y) = egcd a b in
  if g = 0
  then
    if d = 0 then Some Any else None
  else
  if d mod g = 0
  then let k = d / g in
    let (x0, y0) = (k * x, k * y) in
    let (a', b') = (a / gcd a b, -b / gcd a b) in
    assert(a*x0 + b*y0 = d);
    assert(a*b' + b*a' = 0);
    Some (Line ((b', x0), (a', y0)))
  else None
;;

let rec solve_sytem equations vars =
  match equations with
  | [] -> Some (List.map (fun var -> (var, 0)) vars)
  | eq::eqs -> match solve_d eq.a eq.b eq.d with
    | None -> None
    | Some Any -> solve_sytem eqs vars
    | Some (Line ((xk, x0), (yk, y0))) ->
      let new_var  = succ @@ List.fold_left max 0 vars in
      let other_vars = List.filter (fun v -> v != eq.var1 && v != eq.var2) vars in
      let new_vars = new_var :: other_vars in
      let f e = e |> substitite eq.var1 (new_var, xk, x0)
                |> substitite eq.var2 (new_var, yk, y0)
      in
      let new_eqs = List.map f eqs in
      match solve_sytem new_eqs new_vars with
      | None -> None
      | Some sol -> let t = List.assoc new_var sol in
        Some ((eq.var1, xk*t + x0)::(eq.var2, yk*t + y0)::
                List.map (fun v -> (v, List.assq v sol)) other_vars)
;;

let dioph1 (a, b, c, d) =
  let g = gcd a b in
  match solve_d g c d with
  | None -> None
  | Some Any -> failwith "impossible happend"
  | Some (Line ((_, t0), (_, z0))) ->
    match solve_d a b (g*t0) with
    | None -> failwith "impossible happend"
    | Some Any -> Some (0, 0, z0)
    | Some (Line ((_, x0), (_, y0))) -> Some (x0, y0, z0)
;;


let dioph (a, b, c, d) (k, l, m, n) =
  let v = Array.of_list [b*m - c*l; c*k - a*m; a*l - b*k] in
  if v.(0) = 0 && v.(1) = 0 && v.(2) = 0
  then
    if a = 0 && b = 0 && c = 0
    then if d != 0
      then None
      else dioph1 (k, l, m, n)
    else if k = 0 && l = 0 && m = 0
    then if n != 0
      then None
      else dioph1 (a, b, c, d)
    else
      let rec first p = function
        | [] -> failwith "impossible happend"
        | x::xs -> if p x then x else first p xs
      in
      let fnz = first (fun x -> x != 0) in
      if n * fnz [a; b; c] = d * fnz [k; l; m]
      then dioph1 (a, b, c, d)
      else None
  else
    let s = Array.of_list [a*n - k*d; b*n - l*d; c*n - m*d] in
    let vars = [0; 1; 2] in
    let eqs = [{var1 = 1; var2 = 2; a = v.(2); b = -v.(1); d = s.(0)};
               {var1 = 2; var2 = 0; a = v.(0); b = -v.(2); d = s.(1)};
               {var1 = 0; var2 = 1; a = v.(1); b = -v.(0); d = s.(2)};]
    in
    match solve_sytem eqs vars with
    | None -> None
    | Some sol ->
      let (x, y, z) =  (List.assoc 0 sol, List.assoc 1 sol, List.assoc 2 sol) in
      assert(a*x + b*y + c*z = d);
      assert(k*x + l*y + m*z = n);
      Some (x, y, z)
;;

let mkeq (a, b, c) (k, l, m) (x, y, z) =
  ((a, b, c, a*x + b*y + c*z), (k, l, m, k*x + l*y + m*z))
;;

let test () =
  let r () = Random.int 7 in
  let (a, b) = mkeq (r(),r(),r()) (r(),r(),r()) (r(),r(),r()) in
  match dioph a b with
  |None -> print_string "None\n"
  |Some (x, y, z) -> Printf.printf "%d %d %d\n" x y z
;;

Random.self_init ();
for i = 0 to 1000 do
  test()
done

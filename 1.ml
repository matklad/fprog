let rec f n =
    if n = 0 then 1.0 else
    1.0 +. 1.0 /. f (pred n)
;;

let b n =
    let rec aux k =
        float k +. if k = n then 0.0 else 1.0 /. aux (succ k)
    in
    aux 0
;;

let sumsin n =
    let sum_n = float ((n * (n + 1)) / 2) in
    let rec aux acc k =
        if k = n then acc else aux (acc +. sin (float (k+1))) (succ k)
    in
    sin sum_n /. aux 0.0 0
;;

let sumfact n =
    let rec aux acc prod k =
        if k = n + 2 then acc else
        aux (acc + prod) (prod * k) (succ k)
    in
    aux 0 1 2
;;

let nseq n =
    let rec aux m n =
        if n < 0 || m < 0 then 0 else
        if n = 0 then 1 else
        aux (m - 1) n + aux (m - 1) (n - m)
    in
    aux n n
;;

print_float (f 3);
print_newline ();
print_float (b 3);
print_newline ();
print_float (sumsin 2);
print_newline ();
print_int (sumfact 3);
print_newline();
print_int (nseq 9);
print_newline()
;;


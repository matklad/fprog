let g n =
    let primes = ref []
    and sieve = Array.create n true in
    sieve.(0) <- false;
    sieve.(1) <- false;
    for p = 2 to n - 1 do
        if sieve.(p) then
            primes := p :: !primes;
            let i = ref (p * p) in
            while !i < n do
                sieve.(!i) <- false;
                i := !i + p
            done;
    done;
    List.exists (fun p -> let np = n-p in p != np(*sic!*) && sieve.(np)) !primes
;;

let rec gcd a b = if a = 0 then b else gcd (b mod a) a

let c n =
    let rec aux g m n =
        if n < 0 || m < 0 then 0 else
        if n = 0 then 1 else
        aux g (m - 1) n + (if gcd m g = 1 then aux (g * m) (m - 1) (n - m) else 0)
    in
    aux 1 n n


let aux n = print_string(string_of_bool (g n)); print_newline ()
;;
aux 6;
aux 7;
print_int (c 10);
print_newline ()
;;



let rec first_non_zero l = if List.hd l = 0 && List.tl l = [] then 0 else if List.hd l = 0 then first_non_zero (List.tl l) else (List.hd l) + 1;;
let rule1 n = if n mod 2 = 0 then n/2 else 0;;
let rule2 n = if n mod 3 = 0 || n mod 4 == 0 then (if n mod 10 = 0 || ((n mod 100 ) / 10) = 0 then 0 else n - ((n mod 10) * ((n mod 100)/10))) else 0;;
let rule3 n = if n mod 5 = 0 then n - 42 else 0;;
let rec answer n = if n < 42 then 0 else if n = 42 then 1 else first_non_zero (List.sort compare [answer (rule1 n);answer (rule2 n); answer (rule3 n)]);;
let process n = if n = 0 then Printf.printf "BAD LUCK\n" else Printf.printf "%d\n" (n-1);;
let () = process (answer (read_int()));;
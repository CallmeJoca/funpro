
(*Função que procura o primeiro elemento diferente de 0 numa lista, caso todos os elementos sejam iguais a 0 então devolve 0*)
let rec first_non_zero l = if List.hd l = 0 && List.tl l = [] then 0 else if List.hd l = 0 then first_non_zero (List.tl l) else (List.hd l) + 1;;
(*Aplicação das regras 1, 2 e 3 e caso alguma não se aplique a n devolve 0 para esses casos*)
let rule1 n = if n mod 2 = 0 then n/2 else 0;;

let rule2 n = if n mod 3 = 0 || n mod 4 == 0 then (if n mod 10 = 0 || ((n mod 100 ) / 10) = 0 then 0 else n - ((n mod 10) * ((n mod 100)/10))) else 0;;

let rule3 n = if n mod 5 = 0 then n - 42 else 0;;

(*Função que recebe n e chama-se a si própria para os valores obtidos das aplicações das regras, 
  depois ordena estes valores e devolve o valor mais baixo diferente de 0(caso todos os valores sejam 0 então devolve 0)*)
let rec answer n = if n < 42 then 0 else if n = 42 then 1 else 
   first_non_zero (List.sort compare [answer (rule1 n);answer (rule2 n); answer (rule3 n)]);;

(*Recebe o valor devolvido pela função answer e caso esse valor seja 0 (o que significa um fracasso em atingir 42) então escreve BAD LUCK, senão escreve o número de passos realizados menos 1*)
let process n = if n = 0 then Printf.printf "BAD LUCK\n" else Printf.printf "%d\n" (n-1);;

let () = process (answer (read_int()));;
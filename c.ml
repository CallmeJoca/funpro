(*
  Realizado por Jorge Pissarra, nº 39489
  Fontes: https://www.cs.cornell.edu/~kozen/Papers/change.pdf (Obter o valor mais baixo necessário de calcular para saber se as soluções são coincidentes)
          https://backtobackswe.com/platform/content/the-change-making-problem/solutions (Algoritmo dinamico aqui implementado)
*)
let numero_moedas = read_int() - 1;;

let moedas array_moedas tamanho =
  for i = 0 to tamanho do
    Array.set array_moedas i (read_int());
  done;
 array_moedas;;

let array_moedas numero_moedas = moedas (Array.make numero_moedas 0) numero_moedas;;

(*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
let rec maior_moeda x array contador = if (x >= Array.get array contador) then (Array.get array contador) else (maior_moeda x array (contador - 1));;

let rec guloso x array_moedas = if x = 0 then 0 else 1 + guloso (x - (maior_moeda x array_moedas (Array.length array_moedas))) array_moedas;;
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
let minimo x y = if x < y then x else y
(*
  Algoritmo dinamino que gera um vetor com a solução otima para todos os valores de um conjunto de moedas até ao valor x
  Recebe:       x -> valor máximo do vetor;
           moedas -> vetor do conjunto de moedas; 
  Devolve: vetor: -> vetor de tamanho x onde cada valor representa o menor número de moedas necessárias para dar troco correspondente ao indice da posição;
*)
let dinamico x moedas = 
  let vetor = Array.make (x+1) (x+1) in
  Array.set vetor 0 0;
  for i = 0 to x - 1 do
    for j = 0 to (Array.length moedas) - 1 do
      if Array.get moedas j <= i then 
        Array.set vetor i (minimo (Array.get vetor i) ((Array.get vetor (i - Array.get moedas j)) + 1))
    else Array.set vetor i (Array.get vetor i);
  done;
done;
vetor;;
    
let () = 
  let size = numero_moedas in
  let coins = array_moedas size in
  let max = (Array.get coins size) * (Array.get coins (size - 1)) in
  let vetor_dinamico = dinamico max coins in
  for i = 0 to (max - 1) do
    if (guloso i coins) != Array.get vetor_dinamico i then Printf.printf "%d" (i + 1) else ();
  done;;
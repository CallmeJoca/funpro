(*
  Realizado por Jorge Pissarra, nº 39489
  Fontes: https://www.cs.cornell.edu/~kozen/Papers/change.pdf (Obter o valor mais baixo necessário a calcular para saber se as soluções são coincidentes)
          https://backtobackswe.com/platform/content/the-change-making-problem/solutions (Algoritmo dinamico aqui implementado)
*)
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*maior_moeda: função auxiliar à função guloso*)
let rec maior_moeda x array contador = if (x >= Array.get array contador) then Array.get array contador else (maior_moeda x array (contador - 1));;
(*
  Algorítmo guloso que funciona como descrito no enunciado do problema C;
  Recebe:             x -> valor para o qual está a ser calculado o número de moedas necessárias para obter o troco;
           array_moedas -> array que contem os valores das moedas;
  Devolve: numero de moedas necessárias para fazer troco utilizado um algoritmo guloso.
*)
let rec guloso x array_moedas = if x = 0 then 0 else 1 + guloso (x - (maior_moeda x array_moedas ((Array.length array_moedas) - 1))) array_moedas;;
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*minimo: função auxiliar à função dinamico*)
let minimo x y = if x < y then x else y
(*
  Algoritmo dinamino que gera um vetor com a solução otima para todos os valores de um conjunto de moedas até ao valor x
  Recebe:       x -> valor máximo do vetor;
           moedas -> vetor do conjunto de moedas; 
  Devolve: vetor de tamanho x onde cada valor representa o menor número de moedas necessárias para dar troco correspondente ao indice da posição;
*)
let dinamico x moedas = 
  let vetor = Array.make (x) (x+1) in
  Array.set vetor 0 0;
  for i = 0 to x - 1 do
    for j = 0 to (Array.length moedas) - 1 do
      if Array.get moedas j <= i then 
        Array.set vetor i (minimo (Array.get vetor i) ((Array.get vetor (i - Array.get moedas j)) + 1))
    else Array.set vetor i (Array.get vetor i);
  done;
done;
vetor;;
    
(*-----------------------------------------------------------------------------------------------------------------------------------------------------------------------*)
(*moedas: função para receber os valores de cada moeda*)
let moedas array_moedas tamanho =
  for i = 0 to tamanho - 1 do
    Array.set array_moedas i (read_int());
  done;
 array_moedas;;
(*maximo: função para calcular o valor máximo que é necessário para comparar as abordagens "gulosa" e "dinamica"*)
let maximo coins = if Array.length coins = 1 then 1 else (Array.get coins ((Array.length coins) - 1)) * (Array.get coins ((Array.length coins) - 2));;
(*
  resultado: função que recebe um vetor com todos os valores "otimos" de troco até ao valor max e 
            depois compara cada um com a abordagem "gulosa" até descobrir uma falha ou chegar ao fim do array;
  Recebe:     coins -> array com os valores das moedas;
          vetor_din -> array com os valores "ótimos" para criar troco para os valores dos índices;
                max -> valor máximo necessário de calcular;
                  i -> um contador.
*)
let rec resultado coins vetor_din max i = if i = max then Printf.printf "YES\n" else 
  if (guloso i coins) != Array.get vetor_din i then Printf.printf "%d\n" (i) else resultado coins vetor_din max (i+1);;

let () = 
  let size = read_int() in
  let coins = moedas (Array.make size 0) size in
  let max = maximo coins in
  let vetor_dinamico = dinamico max coins in
  resultado coins vetor_dinamico max 0;;
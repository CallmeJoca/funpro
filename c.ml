let ler_num = read_int();;

let rec moedas lista numero = 
  let value = read_int() in
  if numero - 1 = 0 then [value]@lista else
    moedas ([value]@lista) (numero - 1);;

let lista_moedas = moedas [] ler_num;;

let rec maior_moeda x lista_moedas = if x >= List.hd lista_moedas then List.hd lista_moedas else maior_moeda x (List.tl lista_moedas);;

let rec guloso x = if x = 0 then 0 else 1 + guloso(x - maior_moeda x lista_moedas);;



let rec contar_moedas moedas num = 
  if num = (List.nth moedas (List.length moedas)) + (List.nth moedas (List.length moedas - 1)) then 0 else 1;;
    

let () = Printf.printf "%d" (contar_moedas lista_moedas 0);;
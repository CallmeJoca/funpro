let n = read_int;;

let rec passo valor = 
  if valor != 42 then
    if valor > 42 then 
      if valor mod 2 = 0 || valor mod 3 = 0 || valor mod 4 = 0 then 
        if valor mod 2 = 0 then 
          (passo (valor/2)) + 1 else 
              if valor mod 3 = 0 || valor mod 4 = 0 then
          passo ((valor mod 100) + (valor mod 10)) + 1 else 0 else 
            if valor mod 5 = 0 then passo (valor - 42) + 1
    else -1
  else 0;;

let resultado = Printf.printf("%d", passo n);;
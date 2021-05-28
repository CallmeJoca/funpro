type 'a lista = V | C of 'a * 'a lista

(*3 --> 8 --> 5 --> end*)
let li = C ("list",(C ("goes",(C ("brrrrrrrr",V)))))

(*type 'a list = [] | :: of 'a*'a list*)

type 'a listl = 'a lista_ligada ref 
  and 'a lista_ligada = {mutable contents: 'a; next: 'a listl}

type dia = Seg | Ter | Qua | Qui | Sex | Sab | Dom

let dia_util = function
  | Sab | Dom -> false
  | _         -> true

let has2el = function [_;_] -> true | _ -> false

type 'a arv = F | N of 'a * 'a arv list

let rec to_list a = 
  match a with
  | F -> []
  | N (esq,x,dir) -> to_list esq @ x :: to_list dirS
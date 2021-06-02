type  color = W | B (* W: White , B: Black *)
type  image = L of color (* leaf of one  color *)
            | N of image * image * image * image   (* node  with  fourchildren  *)

let ler_ficheiro ficheiro = 
  let f = open_in ficheiro in
  let dimensaox = int_of_char (input_char f) in
  let dimensaoy = int_of_char (input_char f) in
  let matriz = Array.make_matrix dimensaox dimensaoy 0 in
  for i = 0 to dimensaox do
    for j = 0 to dimensaoy do
      matriz.(i).(j) <- ((int_of_char (input_char f)) - 48)
    done;
  done;
  matriz;;

let rec profundidade arvore =
  match arvore with
  | L(color) -> 1
  | N(primeira,segunda,terceira,quarta) -> List.hd (List.sort compare [(profundidade primeira);(profundidade segunda);(profundidade terceira);(profundidade quarta)]) + 1;;

let rec folhas arvore =
  match arvore with
  | L(color) -> 1
  | N(primeira,segunda,terceira,quarta) -> folhas primeira + folhas segunda + folhas terceira + folhas quarta;;

let rec thumbnail arvore = 
  match arvore with
  | L(color) -> [|if color = W then 0 else 1|]
  | N(primeira,segunda,terceira,quarta) -> Array.append (Array.append (thumbnail primeira) (thumbnail segunda)) (Array.append (thumbnail terceira) (thumbnail quarta))

(* 
  Jorge Pissarra, nº 39489
*)
include Printf

type  color = W | B (* W: White , B: Black *)
type  image = L of color (* leaf of one color *)
            | N of image * image * image * image   (* node with four children *)


(*
  ler_matriz: função que lê "imagem" do standard input e guarda esses valores numa matriz
  recebe:     nada;
  devolve:    matriz -> matriz de dimensão especificada no standard input;
*)
let ler_matriz =  (*WORKING*)
  let dimensoes = read_line() in
  let dim = int_of_string (String.sub dimensoes 0 1) in
  let matriz = Array.make_matrix dim dim 0 in
  for i = 0 to dim - 1 do
    let line = read_line() in
    for j = 0 to dim - 1 do
      matriz.(i).(j) <- int_of_string (String.sub line (j*2) 1);
    done;
  done;
  matriz;;

(*
  submatriz: função que recebe uma matriz e quatro limites e devolve a submatriz delimitada por esses valores;
  recebe:    matriz -> matriz original;
             s1     -> limite superior esquerdo;
             s2     -> limite superior direiro;
             s3     -> limite inferior esquerdo;
             s4     -> limite inferior direito;
  devolve:   temp   -> submatriz 1/4 da dimensão original;
*)
let submatriz matriz s1 s2 s3 s4 = (*WORKING*)
  Printf.printf "%d %d %d %d\n" s1 s2 s3 s4;
  for a = 0 to Array.length matriz -1 do
    for b = 0 to Array.length matriz.(a) -1 do
      Printf.printf "%d " matriz.(a).(b);
    done;
    Printf.printf "\n";
  done;
  let tamanho = (Array.length matriz) / 2 in
  let temp = Array.make_matrix tamanho tamanho 0 in
  for i = s1 to s2 do
    for j = s3 to s4 do
      temp.(i - s1).(j - s3) <- matriz.(i).(j);
    done;
  done;
  temp;;
(*
  t_matriz: função que converte uma matriz numa arvore do tipo image;
  recebe:   matriz -> matriz a ser convertida;
  devolve:  N()    -> arvore do tipo image; 
*)
let rec t_matriz matriz = (*FAILING*)
  let dim = Array.length matriz in
  printf "dim = %d\n" dim;
  if dim = 1 then (*this if isn't working*)
    L(if matriz.(0).(0) = 1 then B else W)
  else (*                        V    primeiro quadrante   V                               V      segundo quadrante      V                               V   terceiro quadrante  V                              V         quarto quadrante        V *)
    N(t_matriz (submatriz matriz 0 (dim/2 - 1) 0 (dim/2 - 1)) , t_matriz (submatriz matriz (dim/2) (dim - 1) 0 (dim/2 - 1)) , t_matriz (submatriz matriz 0 (dim/2 - 1) (dim/2) dim), t_matriz (submatriz matriz (dim/2) (dim - 1) (dim/2) (dim - 1)))

(*
  juntar_cores: função que devolve o valor dominante de um quadrante;
  recebe:       a     -> valor color da primeira folha;
                b     -> valor color da segunda folha;
                c     -> valor color da terceira folha;
                d     -> valor color da quarta folha;
  devolve:      color -> valor dominante das quatro cores anteriores;
*)    
let juntar_cores a b c d = (*WORKING?*)
  let w = if a = W then 0 else 1 in
  let x = if b = W then 0 else 1 in
  let y = if c = W then 0 else 1 in
  let z = if d = W then 0 else 1 in
  if w + x + y + z >= 2 then B else W;;

(*
  reduzir_arvore: função que "comprime" uma árvore até que todas as folhas estejam acima do valor dim
  recebe:         arvore -> árvore quaternária a ser podada;
                  dim    -> dimensão final da árvore;
                  prof   -> contador de profundidade de cada chamada recursiva;
  devolve:        arvore reduzida de profundidade dim;
*)
let rec reduzir_arvore arvore dim prof = (*NEEDS TESTING*)
  if prof > dim then
    match arvore with
    | L(cor)                    -> L(cor)
    | N(L(p), L(s), L(t), L(q)) -> L(juntar_cores p s t q)
    | N(a,b,c,d)                -> N(reduzir_arvore a dim (prof + 1), reduzir_arvore b dim (prof + 1), reduzir_arvore c dim (prof + 1), reduzir_arvore d dim (prof + 1))
    else 
    match arvore with
    | L(cor)                    -> L(cor)
    | N(a,b,c,d)                -> N(reduzir_arvore a dim (prof + 1), reduzir_arvore b dim (prof + 1), reduzir_arvore c dim (prof + 1), reduzir_arvore d dim (prof + 1))

let rec profundidade arvore = (*REDUNDANT*)
  match arvore with
  | L(color)                            -> 1
  | N(primeira,segunda,terceira,quarta) -> List.hd (List.sort compare [(profundidade primeira);(profundidade segunda);(profundidade terceira);(profundidade quarta)]) + 1;;

(*
  contar_folhas: função que percorre a árvore a contar o número de folhas e a profundidade da folha mais alta;
  recebe:        arvore -> árvore quaternária a ser percorrida;
                 prof   -> contador de profundidade de cada chamada recursiva;
  devolve:       lista de dois elementos na qual o primeiro elemento representa o número de folhas e o segundo elemento representa a profundidade da folha mais alta;
*)
let rec contar_folhas arvore prof = (*NEEDS TESTING*)
  match arvore with
  | L(color)                            -> [1; prof]
  | N(primeira,segunda,terceira,quarta) -> let p = contar_folhas primeira (prof+1) in
                                           let s = contar_folhas segunda (prof+1)  in 
                                           let t = contar_folhas terceira (prof+1) in 
                                           let q = contar_folhas quarta (prof+1)   in
                                            [ (List.hd p + List.hd s + List.hd t + List.hd q) ;  List.hd (List.hd ( List.sort compare [List.tl p; List.tl s; List.tl t; List.tl q] ) )]


(*
  t_thumbnail: função que faz o inverso de t_matriz;
  recebe:      arvore -> arvore quaternária;
  devolve:     matriz representativa da arvore;
*)
let rec t_thumbnail arvore = (*NEEDS TESTING*)
  match arvore with
  | L(color)                            -> [|[|if color = W then 0 else 1|]|]
  | N(primeira,segunda,terceira,quarta) -> let coluna1 = (Array.append (t_thumbnail primeira) (t_thumbnail terceira)) in
                                           let coluna2 = (Array.append (t_thumbnail segunda) (t_thumbnail quarta))in
                                           Array.append coluna1 coluna2;;

let () = let matriz = ler_matriz in
         let dim = Array.length matriz in
         let profolhas = contar_folhas (reduzir_arvore (t_matriz matriz) dim 0)  0 in
         printf "%d\n%d\n" (List.hd profolhas) (List.hd (List.tl profolhas))
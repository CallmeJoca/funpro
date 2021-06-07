(* Jorge Pissarra, nº 39489 *)
include Printf
type  color = W | B (* W: White , B: Black *)
type  image = L of color (* leaf of one color *)
            | N of image * image * image * image   (* node with four children *)
(*ler_matriz: função que lê "imagem" do standard input e guarda esses valores numa matriz
  recebe:     null;
  devolve:    int array array;*)
let ler_matriz =
  let place_holder = read_line() in
  String.iter (fun x -> ()) place_holder;
  let dim_text = read_line() in
  let dim = int_of_string (String.sub dim_text 0 ((String.length dim_text)/2)) in
  let matriz = Array.make_matrix dim dim 0 in
  for i = 0 to dim - 1 do
    let line = read_line() in
    for j = 0 to dim - 1 do
      matriz.(i).(j) <- int_of_string (String.sub line (j*2) 1);
    done;
  done;
  matriz;;
(*submatriz: função que recebe uma matriz e quatro limites e devolve a submatriz delimitada por esses valores;
  recebe:    int array array -> matriz original;
             int             -> limite superior esquerdo;
             int             -> limite superior direiro;
             int             -> limite inferior esquerdo;
             int             -> limite inferior direito;
  devolve:   int array array;*)
let submatriz matriz s1 s2 s3 s4 =
  let tamanho = (Array.length matriz) / 2 in
  let temp = Array.make_matrix tamanho tamanho 0 in
  for i = 0 to tamanho -1 do
    for j = 0 to tamanho -1 do
      temp.(i).(j) <- matriz.(i + s1).(j + s3);
    done;
  done;
  temp;;
(*soma:    somatório de todos os valores numa matriz;
  recebe:  int array array -> matriz a ser somada;
  devolve: int;*)
let soma matriz = 
  let dim = Array.length matriz in
  let sum = ref 0 in
  for i = 0 to dim - 1 do
    for j = 0 to dim - 1 do
      sum := !sum + matriz.(i).(j);
    done;
  done;
  !sum;;
(*t_matriz: função que converte uma matriz numa arvore do tipo image;
  recebe:   int array array -> matriz a ser convertida;
  devolve:  image;*)
let rec t_matriz matriz =
  let dim = Array.length matriz in
  if (soma matriz) = (dim * dim)|| (soma matriz) = 0 then L(if matriz.(0).(0) = 0 then W else B) else
  match matriz with 
  | [|[|a; b|]; [|c; d|]|] -> N(L(if a = 1 then B else W), L(if b = 1 then B else W),L(if c = 1 then B else W),L(if d = 1 then B else W))
  | _                      -> N(t_matriz (submatriz matriz 0       (dim/2 - 1) 0       (dim/2 - 1)) ,
                                t_matriz (submatriz matriz (dim/2) (dim - 1)   0       (dim/2 - 1)) , 
                                t_matriz (submatriz matriz 0       (dim/2 - 1) (dim/2) dim), 
                                t_matriz (submatriz matriz (dim/2) (dim - 1)   (dim/2) (dim - 1)));;
(*juntar_cores: função que devolve o valor dominante de um quadrante;
  recebe:       color     -> valor color da primeira folha;
                color     -> valor color da segunda folha;
                color     -> valor color da terceira folha;
                color     -> valor color da quarta folha;
  devolve:      color;*)
let juntar_cores a b c d =
  let w = if a = W then 0 else 1 in
  let x = if b = W then 0 else 1 in
  let y = if c = W then 0 else 1 in
  let z = if d = W then 0 else 1 in
  if w + x + y + z >= 2 then B else W;;
(*reduzir: função auxiliar a reduzir_arvore que converte uma arvore numa folha;
  recebe:  image -> arvore a ser convertida;
  devolve: color;*)
let rec reduzir arvore = 
  match arvore with
  | L(cor)                    -> cor
  | N(a,b,c,d)                -> juntar_cores (reduzir a) (reduzir b) (reduzir c) (reduzir d);;
(*reduzir_arvore: função que "comprime" uma árvore até que todas as folhas estejam acima do valor dim
  recebe:         image -> árvore quaternária a ser podada;
                  int   -> dimensão final da árvore;
                  int   -> contador de profundidade de cada chamada recursiva;
  devolve:        image;*)
let rec reduzir_arvore arvore dim prof = (*Failing*)
  if prof = dim then
    match arvore with
    | L(color)                  -> L(color)
    | N(a,b,c,d)                -> L(juntar_cores (reduzir a) (reduzir b) (reduzir c) (reduzir d))
    else 
    match arvore with
    | L(cor)                    -> L(cor)
    | N(a,b,c,d)                -> N(reduzir_arvore a dim (prof + 1),
                                     reduzir_arvore b dim (prof + 1),
                                     reduzir_arvore c dim (prof + 1), 
                                     reduzir_arvore d dim (prof + 1));;
(*contar_folhas: função que percorre a árvore a contar o número de folhas e a profundidade da folha mais alta;
  recebe:        image -> árvore quaternária a ser percorrida;
                 int   -> contador de profundidade mínima da árvore;
                 int   -> contador de profundidade máxima da árvore;
  devolve:       int array;*)
let rec contar_folhas arvore prof_min prof_max =
  match arvore with
  | L(color)                            -> [|1; prof_min;prof_max|]
  | N(primeira,segunda,terceira,quarta) -> let p = contar_folhas primeira (prof_min+1) (prof_max+1) in
                                           let s = contar_folhas segunda (prof_min+1) (prof_max+1) in 
                                           let t = contar_folhas terceira (prof_min+1) (prof_max+1) in 
                                           let q = contar_folhas quarta (prof_min+1) (prof_max+1)  in
                                            [|(p.(0) + s.(0) + t.(0) + q.(0));
                                              List.hd(List.sort compare[p.(1) ; s.(1) ; t.(1) ; q.(1)]);
                                              (Array.of_list (List.sort compare[p.(2) ; s.(2) ; t.(2) ; q.(2)])).(3)|];;
(*print_t: função que gera uma matriz da arvore;
  recebe:  image -> arvore quaternária a ser convertida;
           int   -> dimensão da matriz
  devolve: int array array;*)
  let rec criar_thumbnail arvore dim =
    match arvore with
    | L(c)       -> Array.make_matrix dim dim (if c = B then 1 else 0)
    | N(p,s,t,q) -> let m = Array.make_matrix dim dim (-1) in
                    let q1 = criar_thumbnail p (dim/2) in
                    let q2 = criar_thumbnail s (dim/2) in
                    let q3 = criar_thumbnail t (dim/2) in
                    let q4 = criar_thumbnail q (dim/2) in
                    for a = 0 to (dim/2)-1 do
                      for b = 0 to (dim/2)-1 do
                        m.(a).(b)                 <- q1.(a).(b);
                        m.(a + dim/2).(b)         <- q2.(a).(b);
                        m.(a).(b + dim/2)         <- q3.(a).(b);
                        m.(a + dim/2).(b + dim/2) <- q4.(a).(b);  
                      done;
                    done;
                    m;;
(*nivel:   função auxiliar para calcular o nivel de profundidade aonde será feito o corte
  recebe:  int -> valor da divisão do tamanho da matriz original pelo valor da nova matriz
  devolve: int*)
let nivel dim = if dim = 2 then 1 else 
                if dim = 4 then 2 else
                if dim = 8 then 3 else
                if dim = 16 then 4 else
                if dim = 32 then 5 else
                if dim = 64 then 6 else
                if dim = 128 then 7 else
                if dim = 256 then 8 else
                if dim = 512 then 9 else
                10

let () = let matriz = ler_matriz in
         let dim = int_of_string (read_line()) in
         let arvore_og = t_matriz matriz in
         let profolhas = contar_folhas arvore_og  0 0 in
         let tamanho = profolhas.(2) - (nivel ((Array.length matriz)/dim)) in
         let arvore = reduzir_arvore arvore_og tamanho 0 in
         printf "%d\n%d\n" profolhas.(1) profolhas.(0);
         let thumbnail = criar_thumbnail arvore dim in
         for i = 0 to dim - 1 do
            for j = 0 to dim - 1 do
              printf "%d " thumbnail.(i).(j);
            done;
            printf "\n";
          done;
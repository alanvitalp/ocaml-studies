type arvore = Vz | Raiz of arvore * int * arvore;;


let rec inserir_elem elem arv =
  match arv with
  | Vz -> Raiz(Vz, elem, Vz)
  | Raiz (esq, r, dir) -> if elem < r
                          then Raiz(inserir_elem elem esq, r, dir)
                          else Raiz(esq, r, inserir_elem elem dir);;

let rec inserir_arv (sub_arv: arvore) arv =
  match sub_arv with
  | Vz -> arv
  | Raiz(_, sub_r, _) ->
    match arv with
    | Vz -> sub_arv
    | Raiz (esq, r, dir) -> if sub_r < r
                            then Raiz(inserir_arv sub_arv esq, r, dir)
                            else Raiz(esq, r, inserir_arv sub_arv dir);;

let rec remover_elem elem arv =
  match arv with
  | Vz -> Vz
  | Raiz (esq, r, dir) -> if elem = r
                          then inserir_arv (remover_elem elem dir) (remover_elem elem esq)
                          else Raiz(remover_elem elem esq, r, remover_elem elem dir);;

let rec imprimir_arv arvore =
  match arvore with
  | Vz -> print_string "*"
  | Raiz(Vz, r, Vz) -> print_int r
  | Raiz(esq, r, dir) ->  print_string "[ ";
                          imprimir_arv esq;
                          Printf.printf " -- %d -- " r;
                          imprimir_arv dir;
                          print_string " ]";;


let arvore_ini = Raiz(Raiz(Raiz(Raiz(Vz, 15, Vz), 20, Raiz(Raiz(Vz, 20, Vz), 25, Vz)), 30, Raiz(Vz, 40, Vz)), 50, Raiz(Vz, 60, Vz));;

imprimir_arv arvore_ini;;
print_endline "";;
imprimir_arv (remover_elem 20 arvore_ini);;
print_endline "";;
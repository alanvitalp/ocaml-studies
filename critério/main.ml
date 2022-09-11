let rec selecionar f l = 
  match l with
  | [] -> []
  | h::t -> if f h then h::selecionar f t else selecionar f t;;

let selecionar_pares = selecionar (fun x -> x mod 2 = 0) [-2;-1;0;1;2;3;4;5;6];;

let imprimir_lista l = 
  let rec imprimir l = 
    match l with
    | [] -> ()
    | h::t -> print_int h; print_string " "; imprimir t
  in
  print_string "["; imprimir l; print_string "] \n";;

let imprimir_lista_string l = 
  let rec imprimir l = 
    match l with
    | [] -> ()
    | h::t -> print_string h; print_string " "; imprimir t
  in
  print_string "["; imprimir l; print_string "] \n";;

imprimir_lista (selecionar_pares);;

let rec remover_da_lista x l =
  match l with
  | [] -> []
  | h::t -> if h = x then remover_da_lista x t else h::remover_da_lista x t;;

let lista_sem_x = remover_da_lista 1 [1; 2; 3; 1; 2; 1];;

imprimir_lista (lista_sem_x);;

let separar_tuplas_em_lista l =
  let rec separar l = 
    match l with
    | [] -> ([], [])
    | (a, b)::t -> let (l1, l2) = separar t in (a::l1, b::l2)
  in
  separar l;;


let separar_em_listas = separar_tuplas_em_lista [ (1, "Um"); (2, "Dois"); (3, "Três") ];;


(* falta imprimir a tupla, mas de antemao da pra fazer assim *)
let (l1, l2) = separar_em_listas;;
imprimir_lista l1;;
imprimir_lista_string l2;;



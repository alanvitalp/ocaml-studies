type arvore = Vazia | Raiz of arvore * int * arvore;;

let remover i arv =
  let rec aux i arv =
    match arv with
    | Vazia -> Vazia
    | Raiz (esq, x, dir) ->
      if i < x then Raiz (aux i esq, x, dir)
      else if i > x then Raiz (esq, x, aux i dir)
      else
        match (esq, dir) with
        | (Vazia, Vazia) -> Vazia
        | (Vazia, _) -> dir
        | (_, Vazia) -> esq
        | (_, _) ->
          let rec minimo = function
            | Raiz (Vazia, x, _) -> x
            | Raiz (esq, _, _) -> minimo esq
            | _ -> failwith "minimo: arvore vazia"
          in
          let m = minimo dir in
          Raiz (esq, m, aux m dir)
  in
  aux i arv;;

let arv = Raiz (Raiz (Vazia, 1, Vazia), 2, Raiz (Vazia, 3, Vazia));;

let arv = remover 2 arv;;

let imprimir_arv arv =
  let rec aux arv =
    match arv with
    | Vazia -> ""
    | Raiz (esq, x, dir) -> "(" ^ aux esq ^ string_of_int x ^ aux dir ^ ")"
  in
  print_endline (aux arv);;

imprimir_arv arv;;
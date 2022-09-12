let rec insere x l =
  match l with
  | [] -> [x]
  | h::t -> if x <= h then x::l else h::(insere x t)

let ord_por_insercao l =
  let rec aux l =
    match l with
    | [] -> []
    | h::t -> insere h (aux t)
  in aux l

let rec imprime l =
  match l with
  | [] -> ()
  | h::t -> print_int h; print_string " "; imprime t

let _ = imprime (ord_por_insercao [5; 3; 1; 4; 2])
let lista = [5; 4; 3; 2; 1];;

(* tempo o(n**2) *)
let rec inverter_ing l =
  match l with
  | [] -> []
  | h::t -> (inverter_ing t) @ [h];;

let rec imprimir l =
  match l with
  | [] -> []
  | h::t -> print_int h; print_string " "; imprimir t;;

let lista_inversa = inverter_ing lista;;

imprimir lista_inversa;;

(* tempo o(n) *)
let rec inverter l =
  let rec aux l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> aux t (h::l2)
  in aux l [];;

let lista_inversa = inverter lista;;

print_string "\n";
imprimir lista_inversa;;

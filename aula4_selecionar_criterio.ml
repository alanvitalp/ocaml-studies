let rec selecionar criterio l =
  match l with
  | h::t ->
    if criterio h
      then h :: (selecionar criterio t)
    else (selecionar criterio t)
  | [] -> [];;

let criterio valor =
  valor mod 2 = 0;;

let lista = [1; 2; 3; 4; 6; 7; 5; 7; 4; 3; 2];;

let lista_nova = selecionar criterio lista;;

let rec print_lista lista =
  match lista with
  | h::t -> Printf.printf "%d :: " h;
            print_lista t;
  | [] ->  print_string "[]";;

print_string "[";;
print_lista lista_nova;;
print_string "]\n";;
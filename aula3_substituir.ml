let rec substituir lista a b =
  match lista with
  | h::t ->
    if h = a
    then b :: (substituir t a b)
    else h :: (substituir t a b);
  | [] -> [];;

let a_ = 4;;
let b_ = 5;;

let lista = [1; 2; 3; 4; 6; 7; 5; 7; 4; 3; 2];;

let lista_nova = substituir lista a_ b_;;

let rec print_lista lista =
  match lista with
  | h::t -> Printf.printf "%d :: " h;
            print_lista t;
  | [] ->  print_string "[]";;

print_string "[";;
print_lista lista_nova;;
print_string "]";;

let rec substituir lista func =
  match lista with
  | h::t ->
    (func h) :: (substituir t func)
  | [] -> [];;

let func_test valor =
  if valor = 4
    then 5
  else valor;;
let lista = [1; 2; 3; 4; 6; 7; 5; 7; 4; 3; 2];;

let lista_nova = substituir lista func_test;;

let rec print_lista lista =
  match lista with
  | h::t -> Printf.printf "%d :: " h;
            print_lista t;
  | [] ->  print_string "[]";;

print_string "[";;
print_lista lista_nova;;
print_string "]";;
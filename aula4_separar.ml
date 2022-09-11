let lista_tuplas = [(1, "Um"); (3, "Tres"); (5, "Cinco")];;



let listar_tuplas tupla1 tupla2 =
  let (primeira1, segunda1) = tupla1 in
    let (primeira2, segunda2) = tupla2 in
      (primeira1::primeira2, segunda1::segunda2);;


let rec separar list_tuplas =
  match list_tuplas with
  | [] -> ([], [])
  | h::t ->
    listar_tuplas h (separar t);;



let tupla_listas = separar lista_tuplas;;


let rec print_lista_int lista =
  match lista with
  | h::t -> Printf.printf "%d :: " h;
            print_lista_int t;
  | [] ->  print_string "[]";;

let rec print_lista_str lista =
  match lista with
  | h::t -> Printf.printf "%s :: " h;
            print_lista_str t;
  | [] ->  print_string "[]";;
  


let (primeira, segunda) = tupla_listas in
  print_string "([";
  print_lista_int primeira;
  print_string "], ";
  print_string "[";
  print_lista_str segunda;
  print_string "])\n";;
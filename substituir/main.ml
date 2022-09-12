let read_number message =
  print_string message;
  read_int () in

let list_length = 
  print_string "Digite o tamanho da lista: ";
  read_int () in

let push_number lst = print_string "Insira um número na lista: "; lst @ [read_int ()] in

let rec create_list n =
  match n with
    | 0 -> []
    | _ -> push_number ((create_list (n - 1))) in

let rec swap_a_by_b lst a b =
  match lst with
    | [] -> []
    | hd :: tl ->
      match hd with
        | a -> b :: swap_a_by_b tl a b in

let rec print_list lst =
  match lst with
    | [] -> ()
    | hd :: tl -> print_int hd; print_string " "; print_list tl in

let rec map l f = 
  match l with
  | [] -> []
  | h::t -> f h :: map t f in

let rec swap_func a b = 
  if a = b
  then b
  else a in

let n = list_length in
let list = create_list n in
let a =  read_number "Digite o numero para substituir: " in
let b = read_number "Digite o numero que irá substituir: " in

let result_with_map = map list (fun x -> swap_func a b) in
let result = swap_a_by_b list a b in
(* let result_with_map = List.map (fun x -> if x = a then b else x) list in *)

print_string "\nlist original: ";
print_list list;
print_string "\nlist resultante: ";
print_list result_with_map;

if list = result_with_map then
  print_string "\nlist não alterada! \n"
else
  print_string "\nlist alterada com sucesso! \n";

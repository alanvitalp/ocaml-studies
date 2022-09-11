(* Exercicio em aula *)

print_string "Digite o número de estudantes: ";;
let numero = read_int ();;

let ler_notas n =
  Printf.printf "Digite as notas do estudante Nº %d:\n" (n);
  print_string "Nota AP1: ";
  let ap1 = read_float () in
  print_string "Nota AP2: ";
  let ap2 = read_float () in
  ((ap1 +. ap2) /. 2.);;

let rec formar_lista n =
  if n = 0
  then []
  else ler_notas n :: formar_lista (n-1);;

let lista_formada = formar_lista numero;;

let rec print_medias lista n =
  match lista with
  | h::t ->
    Printf.printf "Nota do estudante Nº%d: %g\n" n h;
    print_medias t (n-1);
  | [] -> print_endline "Fim da lista";;

print_medias lista_formada numero;;
type cadastro = { nome: string; nascimento: string };;

let exibir_menu () = 
  print_endline "\nMenu:";
  print_endline "\tL: Listar o cadastro inteiro";
  print_endline "\tI: Inserir um registro no cadastro";
  print_endline "\tS: Sair";
  print_string "\nOpção: ";
  read_line ();;

let option op = 
  match op with
  | "L" -> print_endline "\nCadastro atual:"
  | "I" -> print_endline "\nLeitura de novo registro:"
  | _ -> print_endline "\nOpção inválida";;

let cadastro_pessoa () =
  print_string "\nNome: ";
  let nome = read_line () in
  print_string "Data de nascimento: ";
  let nascimento = read_line () in
  {nome = nome; nascimento = nascimento}::[];;

let rec exibir_cadastro cadastro =
  match cadastro with
  | [] -> ()
  | {nome = nome; nascimento = nascimento}::t -> 
    print_endline ("Nome: " ^ nome ^ ", Nascimento: " ^ nascimento);
    exibir_cadastro t;;

let rec main cadastro =
  let op = exibir_menu () in
  option op;
  match op with
  | "L" -> exibir_cadastro cadastro; main cadastro
  | "I" -> main (cadastro_pessoa () @ cadastro)
  | "S" -> print_endline "Saindo..."
  | _ -> main cadastro;;

main [];;
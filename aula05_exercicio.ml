type data = {dia: int; mes: int; ano: int};;
type registro = {nome: string; data: data};;

let ler_data () =
  print_string "Digite a data (DD/MM/AAAA): ";
  let s = read_line () in
  let f i n = int_of_string (String.sub s i n) in
    {dia = f 0 2; mes = f 3 2; ano = f 6 4};;

let ler_registro () =
  print_string "Digite o nome: ";
  let nome = read_line () in
    let data = ler_data () in
      {nome = nome; data = data};;

let rec se_existe cadastro registro =
  let nome = registro.nome in
    match cadastro with
    | h::t ->
      (h.nome = nome) || (se_existe t registro);
    | [] -> false;;

let inserir cadastro =
  print_endline "Digite as informações do registro:";
  let registro = ler_registro () in
    if se_existe cadastro registro
    then
      (print_endline "Já existe um cadastro com esse nome";
      cadastro)
    else
      registro::cadastro;;

let rec listar cadastro =
  match cadastro with
  | registro::t ->
    print_endline "------------------------------------------------";
    Printf.printf "Nome: %s\nData: %d/%d/%d\n" registro.nome registro.data.dia registro.data.mes registro.data.ano;
    print_endline "------------------------------------------------";
    listar t;
  | [] ->
    print_endline "Fim do cadastro";;

let cadastro = [];;

let rec menu cadastro =
  print_endline "================================================";
  print_endline "Menu:";
  print_endline "L: Listar o cadastro inteiro";
  print_endline "I: Inserir registro no cadastro";
  print_endline "S: Sair";
  print_string "Digite a opção escolhida (L/I/S): ";
  let comando = read_line () in
    print_endline "================================================";
    match comando with
    | "L" ->  listar cadastro;
              menu cadastro;
    | "I" ->  let cadastro_novo = inserir cadastro in
              menu cadastro_novo;
    | "S" ->  print_endline "Fim";
    | _   ->  menu cadastro;;

let main () =
  menu cadastro;;

main ();;
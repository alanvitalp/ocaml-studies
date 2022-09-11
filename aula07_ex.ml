exception ErroDeEntrada of string;;
exception ErroDeLeitura of string;;

let media (lista_notas : string list) : float =
  let rec processar_notas (sum: float) (count: int) (notas: string list) =
    match notas with
    | ""::_ -> raise (ErroDeEntrada("Linha com valor de nota vazio"))
    | [] -> sum /. (float_of_int count)
    | h::t -> processar_notas (sum +. (float_of_string h)) (count + 1) t in
  processar_notas 0. 0 lista_notas;;


let processar_linha (linha: string list) =
  match linha with
  | ""::_ -> raise (ErroDeEntrada("Não foi informado nome para um estudante/Linha vazia"))
  | _::[] -> raise (ErroDeEntrada("Não foram informadas notas para um estudante"))
  | [] -> raise (ErroDeLeitura("processar_linha recebeu uma lista vazia"))
  | nome::notas -> (nome, string_of_float(media notas));;

let rec processar_arq arq_in arq_out =
  try
    let linha = input_line arq_in in
      let lista_linha = String.split_on_char ',' linha in
        let (nome, media) = processar_linha lista_linha in
          output_string arq_out (nome ^ "," ^ media ^ "\n");
          processar_arq arq_in arq_out;
  with
  | End_of_file ->  close_in arq_in;
                    close_out arq_out;
  | ErroDeLeitura s-> print_endline s;
                      close_in arq_in;
                      close_out arq_out;;



let main () =
  print_string "Digite o nome do arquivo de entrada: ";
  let arq_in = open_in (read_line ()) in
  print_string "Digite o nome do arquivo de saída: ";
  let arq_out = open_out (read_line ()) in
    processar_arq arq_in arq_out;;

main ();;
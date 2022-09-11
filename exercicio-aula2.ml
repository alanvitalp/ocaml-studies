let rec ler_operacao () =
  print_string "Operação:";
  let op = read_line () in
  if op = "+" || op = "-" || op = "*" || op = "/" || op = "**"
  then op
  else
  ler_operacao ();;

let rec ler_nao_nulo () =
  print_string "Float não nulo:";
  let valor = read_float () in
  if valor <> 0.
  then valor
  else ler_nao_nulo ();;

print_endline "NUMERO 1:"
let numero1 = read_float ();;
let op = ler_operacao ();;
print_endline "NUMERO 2:"
let numero2 =
  if op = "/"
  then
  ler_nao_nulo ()
  else
  read_float ();;

if op = "+"
then print_float (numero1 +. numero2)
else if op = "-"
then print_float (numero1 -. numero2)
else if op = "*"
then print_float (numero1 *. numero2)
else if op = "/"
then print_float (numero1 /. numero2)
else if op = "**"
then print_float (numero1 ** numero2)
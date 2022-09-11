let read_float_with_message message =
  print_string message;
  read_float () ;;

let rec read_not_null_float message =
  print_string message;
  let n = read_float () in
  if n <> 0.
  then n
  else read_not_null_float message ;;

let rec read_operation message =
  print_string message;
  let op = read_line () in
  if op = "*" || op = "/" || op = "+" || op = "-" || op = "**"
  then op
  else read_operation (read_operation "Operação inválida, tente novamente: ") ;;

let calculate (x, y, op) =
  if op = "*"
  then x *. y
  else if op = "/"
  then x /. y
  else if op = "+"
  then x +. y
  else if op = "-"
  then x -. y
  else if op = "**"
  then x ** y
  else 0. ;;

let a = read_float_with_message "Digite o primeiro numero: " in
let op = read_operation "Digite a operacao (* ** / + -): " in
let b = read_not_null_float "Digite o segundo numero: " in

let result = calculate (a, b, op) in

Printf.printf "O resultado da operacao é: %.2f\n" result;;

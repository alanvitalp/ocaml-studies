type operador = Sum | Sub ;;

let func_from_operador (op: operador) =
  match op with
  | Sum -> (+.)
  | Sub -> (-.) ;;

type expressao = Valor of float | Exp of expressao * operador * expressao ;;

let rec avaliar_expressao (exp: expressao) =
  match exp with
  | Valor(value) -> value
  | Exp(esq, op, dir) ->
    (func_from_operador op) (avaliar_expressao esq) (avaliar_expressao dir) ;;

let exp = Exp(Exp(Exp(Valor(15.), Sum, Valor(25.)), Sub, Valor(40.)), Sum, Valor(60.));;

Printf.printf "%g\n" (avaliar_expressao exp);;
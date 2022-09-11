type intOuErro = Erro | Inteiro of int ;;
type horarioOuErro = Erro | Horario of int * int ;;

type expr_int =
  | Cti of int
  | Mais of expr_int * expr_int
  | Se of expr_bool * expr_int * expr_int
and
expr_bool =
  | Ctb of bool
  | Conj of expr_bool * expr_bool
  | Igual of expr_int * expr_int ;;

let rec aval_int e =
  match e with
  | Cti i -> i
  | Mais(ei, ej) -> (aval_int ei) + (aval_int ej)
  | Se(eb, ese, esenao) -> 
    if aval_bool eb then aval_int ese
    else aval_int esenao
and aval_bool e =
    match e with
    | Ctb b -> b
    | Conj (ea, eb) -> (aval_bool ea) && (aval_bool eb)
    | Igual(ei, ej) -> (aval_int ei) = (aval_int ej) ;;

(* parte 2 *)
type _ expr =
  | Ctb: bool -> bool expr
  | Conj: (bool expr * bool expr) -> bool expr
  | Igual: ('t expr * 't expr) -> bool expr
  | Cti: int -> int expr
  | Mais: (int expr * int expr) -> int expr
  | Se: (bool expr * 't expr * 't expr) -> 't expr ;;

let rec aval: type t. t expr -> t =
  function
  | Ctb b -> b
  | Conj (ea, eb) -> (aval ea) && (aval eb)
  | Igual (ei, ej) -> (aval ei) = (aval ei)
  | Mais (ei, ej) -> (aval ei) + (aval ej)
  | Se(eb, ese, esenao) -> 
    if aval eb
      then aval ese
      else aval esenao
  | Cti i -> i ;;
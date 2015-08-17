
type exp =
  | X
  | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

exception ERROR

let galculator (expr: exp): float =
  let rec galculator_rec (expr: exp) (v: float): float =
    match expr with
    | X -> v
    | INT n -> float_of_int n
    | REAL r -> r
    | ADD (e1, e2) -> (galculator_rec e1 v) +. (galculator_rec e2 v)
    | SUB (e1, e2) -> (galculator_rec e1 v) -. (galculator_rec e2 v)
    | MUL (e1, e2) -> (galculator_rec e1 v) *. (galculator_rec e2 v)
    | DIV (e1, e2) -> (galculator_rec e1 v) /. (galculator_rec e2 v)
    | SIGMA (a, b, e) ->
        if (galculator_rec a 0.0) > (galculator_rec b 0.0) then
          0.0
        else
          let next = v +. 1.0 in
          (galculator_rec e v) +.
          (galculator_rec (SIGMA (INT (int_of_float next), b, e)) next)
    | INTEGRAL (a, b, e) ->
        if (galculator_rec a 0.0) >= (galculator_rec b 0.0) then
          0.0
        else
          let next = v +. 0.1 in
          (galculator_rec e v) +.
          (galculator_rec (INTEGRAL (REAL next, b, e)) next)
  in
  match expr with
  | X -> raise ERROR
  | SIGMA (a, _, _) -> galculator_rec expr (galculator_rec a 0.0)
  | INTEGRAL (a, _, _) -> galculator_rec expr (galculator_rec a 0.0)
  | _ -> galculator_rec expr 0.0


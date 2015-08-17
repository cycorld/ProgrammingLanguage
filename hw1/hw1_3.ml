
type nat = ZERO | SUCC of nat

let rec natadd (tup: nat * nat): nat =
  let n = fst tup in
  let m = snd tup in
  match m with
  | ZERO -> n
  | SUCC m_pre -> natadd (SUCC n, m_pre)

let rec natmul (tup: nat * nat): nat =
  let n = fst tup in
  let m = snd tup in
  match m with
  | ZERO -> ZERO
  | SUCC m_pre -> natadd (natmul (n, m_pre), n)

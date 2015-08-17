open CommonGrade
open Hw2_2

let h01 = insert (1, EMPTY)
let h02 = insert (3, h01)
let h03 = insert (4, h02)
let h04 = insert (2, h03)
let h05 = insert (5, h04)

let h06 = insert (10, EMPTY)
let h07 = insert (8, h06)
let h08 = insert (7, h07)
let h09 = insert (9, h08)
let h10 = insert (6, h09)

let h11 = insert (13, EMPTY)
let h12 = insert (15, h11)
let h13 = insert (11, h12)
let h14 = insert (14, h13)
let h15 = insert (12, h14)

let lgh1 = merge (h05, h10)
let lgh2 = merge (h15, h10)
let lgh3 = merge (h05, lgh2)

let rec iter (h: heap) (n: int): bool =
  match h with
  | EMPTY -> true
  | NODE _ ->
      if (findMin h) = n then
        iter (deleteMin h) (n + 1)
      else
        false

let _ = check (fun () -> iter h05 1)
let _ = check (fun () -> iter h10 6)
let _ = check (fun () -> iter h15 11)
let _ = check (fun () -> iter lgh1 1)
let _ = check (fun () -> iter lgh2 6)
let _ = check (fun () -> iter lgh3 1)

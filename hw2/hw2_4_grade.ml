open CommonGrade
open Hw2_4

let n1 = INT 3
let n2 = REAL 1.2
let n3 = INT (-2)
let n4 = REAL 0.8
let x1 = ADD (X, INT 1)
let x2 = MUL (X, (MUL (INT 2, X)))
let x3 = SUB (MUL (X, X), INT 1)

let check_eq a b =
  let c = a -. b in
  (c <= 0.00001 && c >= (-0.00001))

let _ = check (fun () -> check_eq (galculator n1) 3.0)
let _ = check (fun () -> check_eq (galculator n2) 1.2)
let _ = check (fun () -> check_eq (galculator n3) (-2.0))
let _ = check (fun () -> check_eq (galculator n4) 0.8)
let _ = check (fun () -> check_eq (galculator (ADD (n1, n2))) 4.2)
let _ = check (fun () -> check_eq (galculator (ADD (ADD (n1, n2), n3))) 2.2)
let _ = check (fun () -> check_eq (galculator (ADD (ADD (n1, n2), n4))) 5.0)
let _ = check (fun () -> check_eq (galculator (SUB (n1, n2))) 1.8)
let _ = check (fun () -> check_eq (galculator (SUB (n4, n3))) 2.8)
let _ = check (fun () -> check_eq (galculator (SUB (SUB (n4, n3), n3))) 4.8)
let _ = check (fun () -> check_eq (galculator (MUL (n1, n2))) 3.6)
let _ = check (fun () -> check_eq (galculator (MUL (ADD (n3, n4), n2))) (-1.44))
let _ = check (fun () -> check_eq (galculator (MUL (n1, (SUB (INT 0, n2))))) (-3.6))
let _ = check (fun () -> check_eq (galculator (DIV (n1, n2))) 2.5)
let _ = check (fun () -> check_eq (galculator (DIV (n4, n3))) (-0.4))
let _ = check (fun () -> check_eq (galculator (SIGMA (INT 1, INT 100, x1))) 5150.0)
let _ = check (fun () -> check_eq (galculator (SIGMA (INT 1, INT 10, x2))) 770.0)
let _ =
  check
  (fun () ->
    check_eq
    (galculator (SIGMA (INT 4, INT 12, MUL ((SUB (X, INT 1)), x1))))
    (galculator (SIGMA (INT 4, INT 12, x3))))

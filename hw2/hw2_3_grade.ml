open CommonGrade
open Hw2_3

let t1 = NODE [LEAF "a"; LEAF "*"; LEAF "b"]
let t2 = NODE [LEAF "c"; LEAF "*"; LEAF "d"]
let t3 = NODE [t1; LEAF "+"; t2]

let loc =
  LOC (LEAF "*", HAND ([LEAF "c"], HAND ([LEAF "+"; t1], TOP, []), [LEAF "d"]))

let _ = check (fun () -> true = true)

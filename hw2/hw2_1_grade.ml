open CommonGrade
open Hw2_1

let m1 =
  AREA ("a", STATION "a")
and m2 =
  AREA ("a", AREA ("a", STATION "a"))
and m3 =
  AREA ("a", AREA ("b", CONNECT (STATION "a", STATION "b")))
and m4 =
  AREA ("a", CONNECT (STATION "a", AREA ("b", STATION "a")))

let m5 =
  AREA ("a", STATION "b")
and m6 =
  AREA ("a", CONNECT (STATION "a", AREA ("b", STATION "c")))
and m7 =
  AREA ("a", AREA ("b", CONNECT (STATION "a", STATION "c")))

let _ = check (fun () -> checkMetro m1 = true)
let _ = check (fun () -> checkMetro m2 = true)
let _ = check (fun () -> checkMetro m3 = true)
let _ = check (fun () -> checkMetro m4 = true)
let _ = check (fun () -> checkMetro m5 = false)
let _ = check (fun () -> checkMetro m6 = false)
let _ = check (fun () -> checkMetro m7 = false)

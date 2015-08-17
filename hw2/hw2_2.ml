
type heap =
  | EMPTY
  | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rec rank (h: heap): rank =
  match h with
  | EMPTY -> 0
  | NODE (r, _, _, _) -> r

let rec findMin (h: heap): value =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, _, _) -> x

let rec shake (x: value) (l: heap) (r: heap): heap =
  if (rank l) < (rank r) then
    NODE ((rank l) + 1, x, r, l)
  else
    NODE ((rank r) + 1, x, l, r)

let rec merge (ht: heap * heap): heap =
  let lh = fst ht in
  let rh = snd ht in
  match lh with
  | EMPTY -> rh
  | NODE (r1, x1, llh, lrh) ->
      (match rh with
      | EMPTY -> lh
      | NODE (r2, x2, rlh, rrh) ->
          if x1 < x2 then
            shake x1 (merge (llh, rh)) lrh
          else
            shake x2 (merge (rlh, lh)) rrh)

let rec insert (p: value * heap): heap =
  merge (snd p, NODE (0, fst p, EMPTY, EMPTY))

let rec deleteMin (h: heap): heap =
  match h with
  | EMPTY -> raise EmptyHeap
  | NODE (_, x, lh, rh) -> merge (lh, rh)


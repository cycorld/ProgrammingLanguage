
type item = string
type tree =
  | LEAF of item
  | NODE of tree list
type zipper =
  | TOP
  | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE

let goLeft (loc: location): location =
  match loc with
  | LOC (t, TOP) -> raise NOMOVE
  | LOC (t, HAND (h::l, z, r)) -> LOC (h, HAND (l, z, t::r))
  | LOC (t, HAND ([], z, r)) -> raise NOMOVE

let goRight (loc: location): location =
  match loc with
  | LOC (t, TOP) -> raise NOMOVE
  | LOC (t, HAND (l, z, h::r)) -> LOC (h, HAND (h::l, z, r))
  | LOC (t, HAND (l, z, [])) -> raise NOMOVE

let goUp (loc: location): location =
  match loc with
  | LOC (t, TOP) -> raise NOMOVE
  | LOC (t, HAND (l, z, r)) ->
      let tlist =
      (match t with
      | LEAF sub -> t::[]
      | NODE sub -> sub) in
      LOC (NODE ((List.rev l)@(tlist@r)), z)

let goDown (loc: location): location =
  match loc with
  | LOC (LEAF t, z) -> raise NOMOVE
  | LOC (NODE (h::t), z) -> LOC (h, HAND ([], z, t))
  | LOC (NODE [], z) -> raise NOMOVE


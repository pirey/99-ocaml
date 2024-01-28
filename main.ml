(* Problem 1 *)
let rec last (xs: 'a list): 'a option =
  match xs with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last rest


(* Problem 2 *)
let rec last_two (xs: 'a list): ('a * 'a) option =
  match xs with
  | [] -> None
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: rest -> last_two rest

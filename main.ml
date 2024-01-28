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

(* Problem 3 *)
let rec at (k: int) (xs: 'a list): 'a option =
  match xs with
  | x :: _ when k == 1 -> Some x
  | _ :: rest when k > 1 -> at (k - 1) rest
  | _ -> None

(* Problem 4 *)
let length (xs: 'a list): int =
  let rec length' (xs: 'a list) (result: int): int =
    match xs with
    | [] -> result
    | _ :: rest -> length' rest (result + 1)
  in length' xs 0


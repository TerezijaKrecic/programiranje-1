(* 1. a) *)
let permutacije a b c = [(a,b,c); (a,c,b); (b,a,c); (b,c,a); (c,a,b); (c,b,a)]

(* 1. b) *)
let zip_opt sez1 sez2 =
  let rec aux s1 s2 acc = match s1, s2 with
    | [], [] -> List.rev acc 
    | x :: xs, [] -> aux xs [] ((Some x, None) :: acc)
    | [], x :: xs -> aux [] xs ((None, Some x) :: acc)
    | x :: xs, y :: ys -> aux xs ys ((Some x, Some y) :: acc)
  in
  aux sez1 sez2 []

(* 1. c) *)
let zip_default sez1 sez2 a1 a2 =
  let rec aux s1 s2 acc = match s1, s2 with
    | [], [] -> List.rev acc 
    | x :: xs, [] -> aux xs [] ((x, a2) :: acc)
    | [], x :: xs -> aux [] xs ((a1, x) :: acc)
    | x :: xs, y :: ys -> aux xs ys ((x, y) :: acc)
  in
  aux sez1 sez2 []
    
(* 1. d) *)

type response = Left | Middle | Right

let distribute f seznam =
  let rec aux sez accright accmiddle accleft = match sez with
    | [] -> (List.rev accright, List.rev accmiddle, List.rev accleft)
    | x :: xs -> let y = f x in
        if y = Right then aux xs (x :: accright) accmiddle accleft
        else if y = Middle then aux xs accright (x :: accmiddle) accleft
        else aux xs accright accmiddle (x :: accleft)
  in
  aux seznam [] [] []

(* 1. e) *)

type ('a, 'b) sum = Left of 'a | Right of 'b

let iso1 (f: ('a, 'b) sum -> 'c) =
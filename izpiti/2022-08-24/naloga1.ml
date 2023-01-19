(* 1. a) *)
let zamenjaj (a, b) (c, d) = ((a, c), (b, d))

(* 1. b) *)
let modus (a, b, c) =
  if a - b = 0 || a - c = 0 then Some a (* dovolj je, da sta dva enaka *)
  else if b - c = 0 then Some b
  else None (* ni več enakih elementov *)

(* 1. c) *)
let uncons sez = match sez with
  | [] -> None
  | x :: xs -> Some (x, xs)

(* 1. d) *)
let rec vstavljaj y sez = match sez with
  | [] | [_] -> sez
  | x :: xs -> x :: y :: vstavljaj y xs

(* 1. e) *)
let popolnoma_obrni sezsez =
  let rec aux_obrni acc sez = match sez with (* navadno obračanje seznama *)
    | [] -> acc
    | x :: xs -> aux_obrni (x :: acc ) xs
  in
  let rec aux_popolnoma_obrni acc sez = match sez with
    | [] -> acc
    | x :: xs -> aux_popolnoma_obrni ((aux_obrni [] x) :: acc) xs
  in
  aux_popolnoma_obrni [] sezsez

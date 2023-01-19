(* 1. a) *)
let je_idempotent ((a, b), (c, d)) =
  if (a*a+b*c = a) && (a*b+b*d = b) && (a*c+c*d = c) && (b*c+d*d = d) then true
  else false

(* 1. b) *)
let produkt seznam =
  let rec aux sez acc = match sez with
    | [] -> acc
    | x :: xs ->
      if x <> 0 then aux xs (x * acc)
      else aux xs acc
  in
  aux seznam 1 

(* 1. c) *)
let stalin_sort sez =
  let rec aux prejsnji preostali = match preostali with
    | [] -> List.rev prejsnji
    | x :: xs ->
      if List.for_all ((>) x) prejsnji then aux (x :: prejsnji) xs
      else aux prejsnji xs
  in
  aux [] sez

(* 1. d) *)
let splosni_collatz f g pogoj z k =
  let rec aux x acc =
    if x = k then List.rev acc
    else if pogoj x then let y = f x in aux y (y :: acc)
    else let y = g x in aux y (y :: acc)
  in
  aux z [z]
let sta_pravokotna (x1, x2, x3) (y1, y2, y3) = x1*y1 + x2*y2 + x3*y3 = 0

let postkompozicija f g x = g (f x)

let dopolni a seznam =
  let rec aux acc sez = match sez with
    | [] -> List.rev acc
    | x :: xs -> match x with
      | None -> aux (a :: acc) xs
      |Some y -> aux (y :: acc) xs
  in
  aux [] seznam

let pretvori b seznam =
  let rec pot a = function (* potenca *)
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pot a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
  in
  let rec aux acc potenca = function
    | [] -> acc
    | x :: xs -> aux (acc + x * (pot b potenca)) (potenca + 1) xs
  in
  aux 0 0 (List.rev seznam)
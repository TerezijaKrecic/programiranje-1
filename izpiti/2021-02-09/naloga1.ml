let pitagorejska_trojica (a, b, c) = a*a + b*b = c*c

let priblizek_korena x =
  (* zaokrožit moramo navzdol *)
  let koren = sqrt (float_of_int x) in
  let zaokrozeno = (Float.round koren) in
  let int_zaokrozeno = int_of_float zaokrozeno in
  if koren -. zaokrozeno > 0. then int_zaokrozeno
  else int_zaokrozeno - 1

let alternirajoci_konstruktorji seznam =
  let aux = el sez match sez with
  | [] -> true
  | x :: xs
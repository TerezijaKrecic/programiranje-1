type 'a operator = 'a -> 'a -> 'a

type 'a atom = Value of 'a | Operator of 'a operator

type 'a term = 'a atom list

type 'a result = Finished of 'a | Term of 'a term | Error

let plus = Operator ( + )

let krat = Operator ( * )

let deljeno = Operator ( / ) (* celi del deljenja *)

let primer : int term = [ Value 3; Value 4; plus; Value 5; deljeno ]    

(* 2. a) *)

let primer1 : int term = [ Value 1; Value 2; plus; Value 4; plus ; Value (- 5); krat ]

let primer2 : float term = [ Value 5.3; Value 4.6; Operator ( /. ); Value 1.7; Operator ( *.) ]

let napacni_primer = [ Value 1; Value 2; plus; krat]

(* 2. b) *)

let korak (izraz : 'a term) = match izraz with
  | [Value a] -> Finished a
  | Value a :: Value b :: Operator f :: xs -> Term (Value (f a b) :: xs)
  | _ -> Error

(* 2. c) *)

let rec izvedi (izraz : 'a term) =
  let naslednji_korak = korak izraz in 
  match naslednji_korak with
    | Finished a -> Some a
    | Error -> None
    | Term term' -> izvedi term'


(* 2. d) *)

let rec valid (izraz : 'a term) = match izraz with
  | [Value a] -> true
  | [] -> true
  | Value a :: Value b :: Operator f :: xs -> valid xs (* na začetku sta 2 vrednosti *)
  | Value b :: Operator f :: xs -> valid xs (* potem pa se vrednosti in operacije izmenjujejo *)
  | _ -> false

(* 2. e) *)

let combine vrednosti operatorji = (* vrednosti mora biti 1 več, sicer ne gre*)
  if List.length vrednosti <> List.length operatorji + 1 then None
  else if List.length vrednosti = 1 then Some (vrednosti) (* če imamo izraz le z vrednostjo *)
  else let rec aux preostale_vr preostali_op acc =
    match preostale_vr, preostali_op with
    | [], [] -> Some (List.rev acc) (* konec *)
    | Value a :: Value b :: xs, _ -> aux (Value b :: xs) preostali_op (Value a :: acc) (* na začetku damo samo ta prvo vrednost noter *)
    | Value a :: xs, Operator f :: xy -> aux xs xy (Operator f :: Value a :: acc) (* vmes; dodajaš v obratnem redu, ker na koncu seznam obrneš *)
    | _ -> failwith "Neki je narobe, ampak ne bi smelo bit ..."
    in
    aux vrednosti operatorji [];;

combine [Value 3; Value 4; Value 5] [Operator (+); Operator (/)];;


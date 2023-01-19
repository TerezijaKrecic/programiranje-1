type 'a list_tree = Leaf of 'a | Node of 'a list_tree list

let example = Node [ Node [Leaf 1]; Leaf 2; Node [Leaf 3; Node[Leaf 5; Leaf 6; Leaf 7]; Leaf 4] ]                       

(* 1. a) *)

let rec map f (drevo : 'a list_tree) = match drevo with
  | Leaf a -> Leaf (f a)
  | Node seznam -> Node ( List.map (map f) seznam )
  
(* 1. b) *)

let count (drevo : 'a list_tree) = match drevo with 
  | Leaf a -> 1 (* robni primer, ko je drevo samo list *)
  | Node seznam_poddreves -> (* če imamo na začetku vozlišče, gremo pogledat za vsakega posebej *)
      let rec prestej_seznam (seznam : 'a list_tree list) acc =
        match seznam with
        | [] -> acc
        | x :: xs -> match x with
          | Leaf a -> prestej_seznam xs (acc + 1)
          | Node sez -> prestej_seznam xs (acc + prestej_seznam sez 0) (* to ni repno rekurzivno *)
      in
      prestej_seznam seznam_poddreves 0

(* 1. c) NISEM UTEGNILA PREVERIT NA PRIMERU *)

let rec apply drevofun drevo = match drevofun, drevo with
  | Leaf f, Leaf a -> Leaf (f a)
  | Node poddrevesafun, Node poddrevesa ->
      let rec zip f sez1 sez2 acc = match sez1, sez2 with
        | [], [] -> List.rev acc
        | x :: xs, y :: ys -> zip f xs ys ((f x y) :: acc)
        | _ -> failwith "seznama nista enako dolga"
      in
      Node ( zip apply poddrevesafun poddrevesa [])
  | _, _ -> failwith "drevesi nista enaki"
          
(* 1. d) NISEM UTEGNILA PREVERIT NA PRIMERU *)
           
(* Podobno kot v c), le da dodamo čez vrednosti še eno funkcijo *)

let rec combine drevofun1 drevofun2 =
  let kompozitum f g = fun x -> f ( g x) in
  match drevofun1, drevofun2 with
  | Leaf f, Leaf g -> Leaf (kompozitum f g)
  | Node poddrevesafun, Node poddrevesa ->
      let rec zip f sez1 sez2 acc = match sez1, sez2 with
        | [], [] -> List.rev acc
        | x :: xs, y :: ys -> zip f xs ys ((f x y) :: acc)
        | _ -> failwith "seznama nista enako dolga"
      in
      Node ( zip combine poddrevesafun poddrevesa [])
  | _, _ -> failwith "drevesi nista enaki"
  
  

(* 1. e) *)

let t1 = Node [ Node [ Leaf (fun x -> x) ]; Leaf (fun x -> x * 2) ]
let t2 = Node [ Leaf 1; Leaf 2 ]
let t3 = Node [ Node []; Leaf 2; Leaf 4 ]

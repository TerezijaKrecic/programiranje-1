type 'a tape = Tape of { left : 'a list; head : 'a; right : 'a list }

type 'a command = Left | Do of ('a -> 'a) | Right

let example = Tape { left = [ 3; 2; 1 ]; head = 4; right = [ 5; 6 ] }

(* 2. a) *)

let map (trak : 'a tape) f  =
  let Tape {left; head; right} = trak in
  Tape {left = List.map f left; head = f head; right = List.map f right}

(* 2. b) *)

let izvedi (trak : 'a tape) (ukaz : 'a command) =
  let Tape {left = l; head = h; right = r} = trak in
  let premik_v_levo l h r = match List.rev l with
    | [] -> None
    | x :: xs -> Some (Tape {left = List.rev xs; head = x; right = h :: r})
  and premik_v_desno l h r = match r with
    | [] -> None
    | x :: xs -> Some (Tape {left = l @ [h]; head = x; right = xs})
  in
  match ukaz with
    | Do f -> Some (Tape {left = l; head = f h; right = r})
    | Left -> premik_v_levo l h r
    | Right -> premik_v_desno l h r

(* 2. c) *)

let rec izvedi_ukaze (trak : 'a tape) seznam_ukazov = match seznam_ukazov with
  | [] -> trak
  | x :: xs -> match izvedi trak x with
    | None -> trak
    | Some trak' -> izvedi_ukaze trak' xs

  
    
(* 2. d) *)

let naberi_in_pretvori (trak : 'a tape) seznam_ukazov =
  let rec aux trak seznamukazov acc =
    let Tape {left = _; head = h; right = _} = trak in
    match seznamukazov with
    | [] -> (List.rev acc, trak)
    | ukaz :: xs -> match izvedi trak ukaz with
      | None -> (List.rev acc, trak)
      | Some trak' ->
        let Tape {left = _; head = h'; right = _} = trak' in
        aux trak' xs ((h, h') :: acc)
  in
  aux trak seznam_ukazov []

(* 2. e) *)

let pripravi_ukaze (trak: 'a tape) f = failwith "TODO"

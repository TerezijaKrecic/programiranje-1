type 'a merkle = List | Vozlisce of 'a vozlisce

and 'a vozlisce = {
levo : 'a merkle;
podatek : 'a;
desno : 'a merkle;
zgostitev : int;
}

type 'a zgostitev = int -> 'a -> int -> int

let primer_h l p d = ((l * 3) + (p * 5) + (d * 7)) mod 11
let primer_plus l p d = l + p + d

let poskusno_drevo : int merkle = Vozlisce {
levo = Vozlisce {
  levo = Vozlisce { levo = List; podatek = 10; desno = List; zgostitev = 6 };
  podatek = 14;
  desno = Vozlisce { levo = List; podatek = 474; desno = List; zgostitev = 5 };
  zgostitev = 2;
};
podatek = 57;
desno = Vozlisce {
  levo = List;
  podatek = 12;
  desno = Vozlisce { levo = List; podatek = 513; desno = List; zgostitev = 2 };
  zgostitev = 8;
  };
zgostitev = 6;
}

(* pomožna funkcija, ki prebere zgostitev *)
let zgornja_zgostitev drevo = match drevo with
  | List -> 0
  | Vozlisce {levo; podatek; desno; zgostitev} -> zgostitev

let rec preveri (f : 'a zgostitev) (drevo : 'a merkle) =
  match drevo with
  | List -> true
  | Vozlisce { levo = l; podatek = p; desno = d; zgostitev = z } ->
    let desna_zgostitev = zgornja_zgostitev d
    and leva_zgostitev = zgornja_zgostitev l in
    if f leva_zgostitev p desna_zgostitev = z then (preveri f l && preveri f d)
    else false

let rec prestej_napacne (f : 'a zgostitev) (drevo : 'a merkle) =
  match drevo with
  | List -> 0
  | Vozlisce { levo = l; podatek = p; desno = d; zgostitev = z } ->
    let desna_zgostitev = zgornja_zgostitev d
    and leva_zgostitev = zgornja_zgostitev l in
    if f leva_zgostitev p desna_zgostitev = z then (prestej_napacne f l) + (prestej_napacne f d)
    else 1 + (prestej_napacne f l) + (prestej_napacne f d)

let rec popravi (f : 'a zgostitev) (drevo : 'a merkle) = (* popravljati je treba od listov navzgor *)
  match drevo with
  | List -> List
  | Vozlisce { levo = List; podatek = p; desno = List; zgostitev = z } -> Vozlisce { levo = List; podatek = p; desno = List; zgostitev = f 0 p 0 }
  | Vozlisce { levo = List; podatek = p; desno = d; zgostitev = z } -> let uredi_d = popravi f d in Vozlisce { levo = List; podatek = p; desno = uredi_d; zgostitev = f 0 p (zgornja_zgostitev uredi_d) }
  | Vozlisce { levo = l; podatek = p; desno = List; zgostitev = z } -> let uredi_l = popravi f l in Vozlisce { levo = uredi_l; podatek = p; desno = List; zgostitev = f (zgornja_zgostitev uredi_l) p 0 }
  | Vozlisce { levo = l; podatek = p; desno = d; zgostitev = z } -> let uredi_d = popravi f d and uredi_l = popravi f l in Vozlisce { levo = uredi_l; podatek = p; desno = uredi_d; zgostitev = f (zgornja_zgostitev uredi_l) p (zgornja_zgostitev uredi_d) }

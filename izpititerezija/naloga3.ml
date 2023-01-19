let breg1, breg2 =
  ( [|
      [| 60; 50; 40; 30; 20 |];
      [| 40; 50; 60; 73; 80 |];
      [| 10; 20; 30; 40; 50 |];
    |],
    [|
      [| 30; 40; 50; 60; 70 |];
      [| 40; 60; 30; 20; 40 |];
      [| 10; 20; 90; 40; 50 |];
    |] )

(* prehod med bregovoma je le en, tako da to lahko razčlenimo na 2 podproblema. Najprej poiščimo, koliko energije max. mu ostane na prvem bregu (pot od 10 do 20 zgoraj desno) *)

(* nisem utegnila uporabiti memoizacije *)
    
let energija_prvi_breg breg =
    let sirina = Array.length breg.(0) - 1 (* maks. indeks stolpca *)
    and visina = Array.length breg - 1 in (* makx. indeks vrstice *)
    let start = breg.(visina).(0) in (* spodaj levo = 10 *)
    let rec sprehod e x y = (* e = energija na tem mestu, x = pozicija po stolpcih, y = pozicija po vrsticah *)
      if x = sirina && y = 0 then e (* če smo zgoraj desno, damo energijo *)
      else
        let korak_desno = if x + 1 > sirina then 0 else sprehod (e - 10 + breg.(y).(x+1)) (x + 1) y
        and korak_navzgor = if y - 2 < 0 || x - 1 < 0 then 0 else sprehod (e - 12 + breg.(y-2).(x-1)) (x - 1) (y - 2)
        and korak_diag = if y - 1 < 0 then 0 else sprehod (e - 14 + breg.(y-1).(x)) x (y - 1)
        in max (max korak_desno korak_navzgor) korak_diag
    in sprehod start 0 visina

(* podobno naredimo še drugi breg, le da hodimo dol in desno *)
(* lahko bi samo drugi breg obrnili na glavo in uporabili prvo funkcijo, gl. spodaj *)
let energija_drugi_breg breg =
  let sirina = Array.length breg.(0) - 1 (* maks. indeks stolpca *)
  and visina = Array.length breg - 1 in (* makx. indeks vrstice *)
  let start = breg.(0).(0) in (* zgoraj levo = 30 *)
  let rec sprehod e x y = (* e = energija na tem mestu, x = pozicija po stolpcih, y = pozicija po vrsticah *)
    if x = sirina && y = visina then e
    else
      let korak_desno = if x + 1 > sirina then 0 else sprehod (e - 10 + breg.(y).(x+1)) (x + 1) y
      and korak_navzdol = if y + 2 > visina || x - 1 < 0 then 0 else sprehod (e - 12 + breg.(y+2).(x-1)) (x - 1) (y + 2)
      and korak_diag = if y + 1 > visina then 0 else sprehod (e - 14 + breg.(y+1).(x)) x (y + 1)
      in max (max korak_desno korak_navzdol) korak_diag
  in sprehod start 0 0

let vsa_energija breg1 breg2 = energija_prvi_breg breg1 - 10 + energija_drugi_breg breg2 (* odštejemo energijo koraka v desno *)
let vsA_energija_z_le_eno_funkcijo breg1 breg2 = let obrnjen_breg = (Array.of_list (List.rev (Array.to_list breg2))) in  energija_prvi_breg breg1 - 10 + energija_prvi_breg  obrnjen_breg

(* obakrat dobim 553 *)

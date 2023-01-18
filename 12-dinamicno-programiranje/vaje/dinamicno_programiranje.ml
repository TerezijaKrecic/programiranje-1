(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)
let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

let odstrani_prvo_vrstico arr =
   (* odstrani prvi element arraya *)
   if Array.length arr = 1 then [| |] (* če imamo le še eno vrstico, vrne prazen array *)
   else Array.init (Array.length arr - 1) (fun i -> arr.(i+1))

let odstrani_prvi_stolpec arr =
   if Array.length arr.(0) = 1 then [| |] (* če imamo le še en stolpec, vren prazen array *)
   else Array.init (Array.length arr) (fun i -> odstrani_prvo_vrstico arr.(i))

let rec max_cheese arr =
   if arr = [| |] then 0
   else arr.(0).(0) + max (max_cheese (odstrani_prvo_vrstico arr)) (max_cheese (odstrani_prvi_stolpec arr))


(* PROBEJMO NA DRUGAČEN NAČIN, BREZ PODMATRIK, Z 2 INDEKSOMA, KI POVESTA, V KATEREM STOLPCU IN VRSTICI SE NAHAJAM *)
let rec aux_max_cheese_ind arr v s =
   let visina = (Array.length arr) - 1 (* max. indeks vrstic *)
   and sirina = (Array.length arr.(0)) - 1 (* max. indeks stolpcev *) in
   let trenutno = arr.(v).(s) in
   let spodnja = if (v + 1) > visina then 0 else aux_max_cheese_ind arr (v + 1) s
   and desna = if (s + 1) > sirina then 0 else aux_max_cheese_ind arr v (s + 1)
   in trenutno + max desna spodnja

let max_cheese_ind arr = aux_max_cheese_ind arr 0 0

(*----------------------------------------------------------------------------*]
 Poleg količine sira, ki jo miška lahko poje, jo zanima tudi točna pot, ki naj
 jo ubere, da bo prišla do ustrezne pojedine.

 Funkcija [optimal_path] naj vrne optimalno pot, ki jo mora miška ubrati, da se
 čim bolj nažre. Ker je takih poti lahko več, lahko funkcija vrne poljubno.
 Pripravite tudi funkcijo [convert_path], ki pot pretvori v seznam tež sirčkov
 na poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # optimal_path test_matrix;;
 - : mouse_direction list = [Right; Down; Right; Down; Right]
 # optimal_path test_matrix |> convert_path test_matrix;;
 - : int list = [1; 2; 4; 5; 1]
[*----------------------------------------------------------------------------*)

type mouse_direction = Down | Right

let optimal_path arr =
   let visina = (Array.length arr) - 1 (* max. indeks vrstic *)
   and sirina = (Array.length arr.(0)) - 1 (* max. indeks stolpcev *) in
   let rec aux_optimal_path arr v s sez =
      if v > visina || s > sirina then List.rev sez
      else
         let vsota_spodnje_matrike = if (v + 1) > visina then 0 else aux_max_cheese_ind arr (v + 1) s
         and vsota_desne_matrike = if (s + 1) > sirina then 0 else aux_max_cheese_ind arr v (s + 1) in
         if vsota_spodnje_matrike > vsota_desne_matrike then aux_optimal_path arr (v+1) s (Down :: sez)
         else aux_optimal_path arr v (s+1) (Right :: sez)
   in
   aux_optimal_path arr 0 0 []

let convert_path arr pot =
   let rec aux_convert_path arr pot v s sez =
      match pot with
      | [] -> List.rev sez
      | x :: xs ->
         if x = Right then aux_convert_path arr xs v (s + 1) (arr.(v).(s) :: sez)
         else aux_convert_path arr xs (v + 1) s (arr.(v).(s) :: sez)
   in
   aux_convert_path arr pot 0 0 []

(* ŠE NA NAČIN Z INDEKSI *)
(*
let optimal_path_ind arr =
   let visina = (Array.length arr) - 1 (* max. indeks vrstic *)
   and sirina = (Array.length arr.(0)) - 1 (* max. indeks stolpcev *) in
   let rec opt_ind arr v s =
      let trenutno = arr.(v).(s) in
      let desna_vsota, desna_pot = if (v + 1) > visina then (0, []) else opt_ind arr (v + 1) s
      and spodnja_vsota, spodnja_pot = if (s + 1) > sirina then (0, []) else opt_ind arr v (s + 1)
      in
      if desna_vsota > spodnja_vsota then 

   in
   opt_ind arr 0 0
*)   

(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)

let alternating_towers height =
   let rec redbottom height =
      match height with
      | x when x <= 0 -> 0
      | 1 -> 1
      | 2 -> 1
      | _ -> bluebottom (height - 1) + bluebottom (height - 2)
   and
   bluebottom height =
      match height with
      | x when x < 2 -> 0
      | 2 -> 1
      | 3 -> 2
      | _ -> redbottom (height - 2) + redbottom (height - 3)
   in
   redbottom height + bluebottom height

(*----------------------------------------------------------------------------*]
 Izračunali smo število stolpov, a naše vrle gradbince sedaj zanima točna
 konfiguracija. Da ne pride do napak pri sestavljanju, bomo stolpe predstavili
 kar kot vsotne tipe. 

 Stolp posamezne barve so temelji (Bottom), ali pa kot glava bloka pripadajoče
 barve in preostanek, ki je stolp nasprotne barve.

 Definirajte funkcijo [enumerate_towers], ki vrne seznam vseh stolpov podane
 dolžine. Stolpe lahko vrne v poljubnem vrstnem redu. Funkcija naj hitro (in
 brez) prekoračitve sklada deluje vsaj do višine 20.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # enumerate_towers 4;;
 - : tower list = 
    [Red (TopRed (Red2, TopBlue (Blue2, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue3, RedBottom)));
     Red (TopRed (Red1, TopBlue (Blue2, TopRed (Red1, BlueBottom))));
     Blue (TopBlue (Blue3, TopRed (Red1, BlueBottom)));
     Blue (TopBlue (Blue2, TopRed (Red2, BlueBottom)))]
[*----------------------------------------------------------------------------*)

type blue_block = Blue3 | Blue2
type red_block = Red2 | Red1

type red_tower = TopRed of red_block * blue_tower | RedBottom
and blue_tower = TopBlue of blue_block * red_tower | BlueBottom

type tower = Red of red_tower | Blue of blue_tower

let rec add_red red_block = List.map (fun t -> TopRed (red_block, t))
let rec add_blue blue_block = List.map (fun t -> TopBlue (blue_block, t))

let enumerate_towers height =
  let rec redtop height =
    if height < 0 then []
    else if height = 0 then [RedBottom]
    else
        add_red Red1 (bluetop (height - 1)) 
        @ add_red Red2 (bluetop (height - 2))
  and bluetop height =
    if height < 0 || height = 1 then [] 
    else if height = 0 then [BlueBottom]
    else 
      add_blue Blue2 (redtop (height - 2)) 
      @  add_blue Blue3 (redtop (height - 3))
  in
  List.map (fun t -> Red t) (redtop height)
  @ List.map (fun t -> Blue t) (bluetop height)
(*----------------------------------------------------------------------------*]
 Vdrli ste v tovarno čokolade in sedaj stojite pred stalažo kjer so ena ob
 drugi naložene najboljše slaščice. Želite si pojesti čim več sladkorja, a
 hkrati poskrbeti, da vas ob pregledu tovarne ne odkrijejo. Da vas pri rednem
 pregledu ne odkrijejo, mora biti razdalija med dvema zaporednima slaščicama,
 ki ju pojeste vsaj `k`.

 Napišite funkcijo [ham_ham], ki sprejme seznam naravnih števil dolžine `n`, ki
 predstavljajo količino sladkorja v slaščicah v stalaži in parameter `k`,
 najmanjšo razdalijo med dvema slaščicama, ki ju še lahko varno pojeste.
 Funkcija naj vrne seznam zastavic `bool`, kjer je `i`-ti prižgan natanko tedaj
 ko v optimalni požrtiji pojemo `i`-to slaščico.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # ham_ham test_shelf 1;;
 - : bool list = [false; true; false; true; false; true; false; true; false]
 # ham_ham test_shelf 2;;
 - : bool list = [false; true; false; false; false; true; false; false; false]
[*----------------------------------------------------------------------------*)

let test_shelf = [1;2;-5;3;7;19;-30;1;0]

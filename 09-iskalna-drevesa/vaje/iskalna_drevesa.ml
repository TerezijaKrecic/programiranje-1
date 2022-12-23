(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a drevo =
     | Prazno
     (* | Sestavljeno of 'a drevo * 'a * 'a drevo *)
     | Sestavljeno of {
          levo: 'a drevo;
          vrednost: 'a;
          desno: 'a drevo;
     }

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)
let leaf x = Sestavljeno {levo=Prazno; vrednost=x; desno=Prazno}
let test_tree = Sestavljeno {
     levo = (Sestavljeno {
          levo = (leaf 0);
          vrednost=2;
          desno=Prazno
          });
     vrednost=5;
     desno = (Sestavljeno {
          levo = (leaf 6);
          vrednost=7;
          desno = (leaf 11)
          })
     }

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)
let rec mirror drevo = match drevo with
     | Prazno -> Prazno
     | Sestavljeno {levo; vrednost; desno} -> Sestavljeno {levo = (mirror desno); vrednost; desno = (mirror levo)}

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)
let rec height drevo =
     match drevo with
     | Prazno -> 0
     | Sestavljeno {levo; vrednost; desno} -> 1 + max (height levo) (height desno)

let rec size drevo =
     match drevo with
     | Prazno -> 0
     | Sestavljeno {levo; vrednost; desno} -> 1 + size levo + size desno
(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)
let rec map_tree f drevo = match drevo with
     | Prazno -> Prazno
     | Sestavljeno {levo; vrednost; desno} ->
          Sestavljeno {
               levo = (map_tree f levo);
               vrednost = (f vrednost);
               desno = (map_tree f desno)
               }

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)
let rec list_of_tree drevo =
     match drevo with
     | Prazno -> []
     | Sestavljeno {levo; vrednost; desno} ->
          (list_of_tree levo) @ [vrednost] @ (list_of_tree desno)
          (*
          List.sort compare (
          vrednost :: (
               List.append (list_of_tree levo) (list_of_tree desno)
               )
          )
          *)
(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let is_bst drevo =
     let seznam = list_of_tree drevo in
     seznam = List.sort compare seznam

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec insert x drevo =
     match drevo with
     | Prazno -> Sestavljeno {levo=Prazno; vrednost=x; desno=Prazno}
     | Sestavljeno {levo; vrednost; desno} ->
          if x < vrednost then Sestavljeno {levo=(insert x levo); vrednost; desno}
          else if x > vrednost then Sestavljeno {levo; vrednost; desno=(insert x desno)}
          else drevo

(* TA PREVERI, ČE JE ELEMENT V SEZNAMU ELEMENTOV
let member x drevo =
     let elementi = list_of_tree drevo in
     List.mem x elementi
*)

let rec member x drevo =
     match drevo with
     | Prazno -> false
     | Sestavljeno {levo; vrednost; desno} ->
          if x = vrednost then true
          else if x < vrednost then member x levo
          else member x desno

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)
let rec member2 x drevo =
     match drevo with
     | Prazno -> false
     | Sestavljeno {levo; vrednost; desno} -> (member2 x levo) || x = vrednost || (member2 x desno)

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Sestavljeno {levo=Prazno; vrednost=5; desno=(leaf 7)});;
 - : int option = None
[*----------------------------------------------------------------------------*)
let succ drevo = match drevo with
     | Prazno -> None
     | Sestavljeno {levo; vrednost; desno} ->
          match List.sort compare (list_of_tree desno) with
          | [] -> None
          | x :: xs -> Some x

let pred drevo = match drevo with
     | Prazno -> None
     | Sestavljeno {levo; vrednost; desno} ->
          match List.rev (List.sort compare (list_of_tree levo)) with
          | [] -> None
          | x :: xs -> Some x        

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)
let rec delete x drevo =
     match drevo with
     | Prazno -> Prazno
     | Sestavljeno {levo; vrednost; desno} ->
          if x < vrednost then Sestavljeno{levo=(delete x levo); vrednost; desno}
          else if x > vrednost then Sestavljeno{levo; vrednost; desno=(delete x desno)}
          else
               match levo with
               | Prazno -> desno
               | Sestavljeno {levo=levo1; vrednost=nasl; desno=_} -> Sestavljeno {levo=Prazno; vrednost=nasl; desno}

(*
           5
          / \
         2   7
        /   / \
       0   6   11
*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type ('key, 'value) dict = ('key * 'value) drevo

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)
let test_dict = Sestavljeno {
     levo = Sestavljeno {levo=Prazno; vrednost=("a", 1); desno=Prazno};
     vrednost = ("b", 1);
     desno = Sestavljeno {
          levo=Sestavljeno{levo=Prazno; vrednost=("c", -2); desno=Prazno};
          vrednost=("d",2);
          desno=Prazno
     };
}

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)


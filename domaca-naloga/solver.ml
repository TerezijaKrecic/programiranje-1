type available = { loc : int * int; possible : int list }

(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = {
  problem : Model.problem;
  current_grid : (int option) Model.grid;
  current_row: int; (* current_row pove, da so vrstice nad njo že polne *)
  current_column: int;
  current_element_option: int list (* možna števila za prvi zgornji levi manjkajoči element *)
  } 

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

let initialize_state (problem : Model.problem) : state = {
  problem;
  current_grid = Model.copy_grid problem.initial_grid;
  current_row = 0;
  current_column = 0;
  current_element_option = []
  }

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid (* preveri, ali vsebuje kak None *)
  in
  if unsolved then Unsolved state (* če vsebuje None, je stanje nerešeno *)
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in (* vse elemente (ki so oblike Some int) pretvori v int*)
    if Model.is_valid_solution state.problem solution then Solved solution (* če je rešitev ok, potem je to to *)
    else Fail state

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)
  let trenutno_stanje = state.current_grid in
  let rec poisci_prvo_nepolno_vrstico grid v = (* v = indeks vrstice *)
    match v with
    | x when x < 10 -> let v_ta_vrstica = Model.get_row grid v in
      if Array.mem None v_ta_vrstica then (v_ta_vrstica, v) (* če vrstica vsebuje None *)
      else poisci_prvo_nepolno_vrstico grid (v + 1)
    | _ -> failwith "Sudoku je že rešen??"
  in
  let vrstica, v = poisci_prvo_nepolno_vrstico trenutno_stanje state.current_row in (* dobimo array s prvo nepopolno vrstico ter njen indeks *)
  let rec najdi_indeks_stolpca vrstica s =
    match s with
    | x when x < 10 -> if vrstica.(x) = None then s else najdi_indeks_stolpca vrstica (s + 1)
    | _ -> failwith "vrstica je polna, napaka v kodi"
  in
  let s = najdi_indeks_stolpca vrstica (if v = state.current_row then state.current_column else 0) in (* dobimo indeks stolpca, torej trenutno_stanje.(v).(s) = None *)
  (* poiščimo števila, ki manjkajo v vrstici *)
  let rec poisci_manjkajoca_stevila grid v s acc t =
    match t with
    | x when 0 < x && x < 10 ->
      if
        Array.mem (Some x) (Model.get_row grid v) || (* če je element vsebovan v vrstici/stolpcu/škatli, gremo naprej *)
        Array.mem (Some x) (Model.get_column grid s) ||
        Array.mem (Some x) (Model.get_box grid (Model.box_ind v s))
        then poisci_manjkajoca_stevila grid v s acc (t + 1)
      else poisci_manjkajoca_stevila grid v s (x::acc) (t + 1)
    | _ -> List.rev acc
  in
  let manjkajoca = (poisci_manjkajoca_stevila trenutno_stanje v s [] 1) in
  (* KAJ IMAMO DO SEDAJ: 
  v, s - indeksa prvega zgornjega in levega manjkajočega elementa
  manjkajoca - seznam manjkajocih stevil v v-ti vrstici, urejen po velikosti   
  *)
  if manjkajoca = [] then None
  else
    let prvi = List.hd manjkajoca and ostali = List.tl manjkajoca in
    let st1 = {
      problem = state.problem;
      current_grid = Model.copy_grid trenutno_stanje;
      current_row = v;
      current_column = s;
      current_element_option = [prvi]
    }
    (* st1 je del, kjer namesto prvega None elementa (v explore_state) zapišemo prvo trenutno legalno število;
       st2 vsebuje ostale možnosti *)
    and st2 = {
      problem = state.problem;
      current_grid = Model.copy_grid trenutno_stanje;
      current_row = v;
      current_column = s;
      current_element_option = ostali
    } in
    Some (st1, st2)


(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)
  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      st1.current_grid.(st1.current_row).(st1.current_column) <- (Some (List.hd st1.current_element_option));
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          let rec resi st = (* vstavimo prvi naslednji element namesto None in pogledamo, če reši, sicer gremo naprej po seznamu možnosti za tisti elemente grida *)
            match st.current_element_option with
            | [] -> None (* failwith "Nekaj je slo narobe, noben element ne ustreza" *)
            | x::xs ->
              st.current_grid.(st.current_row).(st.current_column) <- (Some x);
              match solve_state st with
              | Some solution -> Some solution
              | None -> resi {st with current_element_option = xs} 
          in
          resi st2)

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state

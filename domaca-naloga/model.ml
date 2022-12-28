(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = ('a Array.t) Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
(* PRIMER: chunkify 2 [1;2;3;4;5] -> [[1; 2]; [3; 4]; [5]] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep
(* PRIMER: string_of_list Int.to_string "-" [1;2;3] -> "1-2-3" *)

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep
(* string_of_nested_list Int.to_string "-" "|" [[1;2];[3;4;5]] -> "1-2|3-4-5" *)

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"
(* string_of_row Int.to_string [|1;2;3;4;5;6;7;8;9|] -> "┃123│456│789┃\n" *)

let print_grid string_of_cell grid =
  (* grid je oblike Array of Arrays (9x Array po 9 elementov), spravi v obliko 3*3 mreže, kot je v mapi s primeri sudokujev *)
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big


(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind)
(* iz grida prebere (int+1)-ti Array oz. vrstico *)

let rows grid = List.init 9 (get_row grid)
(* vrne seznam z Array-i/vrsticami *)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))
(* vrne Array z elementi v tem stolpcu *)

let columns grid = List.init 9 (get_column grid)
(* vrne seznam z Array-i/stolpci *)

let get_box (grid : 'a grid) (box_ind : int) =
  let tretjina box_ind mod 3 in
  (* spr. tretjina označuje, katera skupina treh stolpcev: 0 -> prvi trije; 1 -> drugi trije; 2 -> tretji trije:
     0 -> 0,1,2
     1 -> 3,4,5
     2 -> 6,7,8 *)
  let seznam_stolpcev = Array.init 3 (fun i -> get_column grid (tretjina*3 + i)) in
  (* to vrne Array z Arrayi pravih stolpcev, od katerih moramo samo še vzeti prave elemente ven *)
  let vzemi_tri_elemente arr ind = Array.init 3 (fun i -> arr.(ind + i)) in
  (* sprejme array in prvi indeks, kjer vzame le prve tri elemente od vključno indeksa dalje*)
  if box_ind < 3 then indeks = 0
  else if box_ind < 6 then indeks = 3
  else indeks = 6 in
  Array.concat List.init 3 (fun i -> vzemi_tri_elemente seznam_stolpcev.(i) indeks)
  (* dobimo Array z 9-imi elementi škatle - prve, druge in tretje vrstice*)

let boxes grid = List.init 9 (get_box grid)
(* vrne seznam z Arrayi posameznih škatel *)


(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = Array.init 9 (Array.map f (get_row grid))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let print_problem problem : unit = failwith "TODO"

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = failwith "TODO"

let is_valid_solution problem solution = failwith "TODO"

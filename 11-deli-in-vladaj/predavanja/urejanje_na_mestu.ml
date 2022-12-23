<<<<<<< HEAD
let zamenjaj tabela i j =
  let t = tabela.(i) in
  tabela.(i) <- tabela.(j);
  tabela.(j) <- t

let pivotiraj_na_mestu tabela i0 j0 =
  let pivot = tabela.(i0) in
  let i = ref (i0 + 1)
  and j = ref j0 in
  while !i < !j do
    while !i < !j && tabela.(!i) <= pivot do
      incr i
    done;
    while !i < !j && tabela.(!j) > pivot do
      decr j
    done;
    zamenjaj tabela !i !j
  done;
  let p = if tabela.(!i) <= pivot then !i else !i - 1 in
  zamenjaj tabela i0 p;
  p

let hitro_uredi_na_mestu tabela =
  let rec uredi_med_indeksoma i j =
    if i < j then
      let p = pivotiraj_na_mestu tabela i j in
      uredi_med_indeksoma i (p - 1);
      uredi_med_indeksoma (p + 1) j
  in
  uredi_med_indeksoma 0 (Array.length tabela - 1)
=======
let zamenjaj tabela i j =
  let t = tabela.(i) in
  tabela.(i) <- tabela.(j);
  tabela.(j) <- t

let pivotiraj_na_mestu_z_leve_in_desne tabela zacetek konec =
  let pivot = tabela.(zacetek) and l = ref (zacetek + 1) and d = ref konec in
  while !l < !d do
    while !l < !d && tabela.(!l) <= pivot do
      incr l
    done;
    while !l < !d && tabela.(!d) > pivot do
      decr d
    done;
    zamenjaj tabela !l !d
  done;
  let p = if tabela.(!l) <= pivot then !l else !l - 1 in
  zamenjaj tabela zacetek p;
  p

let pivotiraj_na_mestu_kot_snezni_plug tabela zacetek konec =
  let pivot = tabela.(zacetek) and zacetek_vecjih = ref (zacetek + 1) in
  for i = zacetek + 1 to konec do
    if tabela.(i) <= pivot then (
      zamenjaj tabela i !zacetek_vecjih;
      incr zacetek_vecjih)
  done;
  let p = !zacetek_vecjih - 1 in
  zamenjaj tabela zacetek p;
  p

let hitro_uredi_na_mestu tabela =
  let rec uredi_med_indeksoma i j =
    if i < j then (
      let p = pivotiraj_na_mestu_kot_snezni_plug tabela i j in
      uredi_med_indeksoma i (p - 1);
      uredi_med_indeksoma (p + 1) j)
  in
  uredi_med_indeksoma 0 (Array.length tabela - 1)
>>>>>>> 59dd0a717f432890e416332abbc86404fbd0b217

<<<<<<< HEAD
let f sez =
    let sez = 1 :: sez in
    List.length sez

let g sez =
    f sez + f sez
=======
let sez = [1; 2; 3]

let f x =
    let sez = x :: sez in
    List.length sez

let g x =
    f x + f x
>>>>>>> c456325d506f9e9476ca6aafba18d77c7556f801

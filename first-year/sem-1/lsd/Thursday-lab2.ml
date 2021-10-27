(* discutii tema *)

(* recursivitate *)

let rec suma n = 
  if n = 0 then 0
  else n + suma (n - 1)

  (* suma 1 = 1 + suma 0 = 1 + 0 *)

  (* suma 3 = 3 + suma 2
     suma 2 = 2 + suma 1
     suma 1 = 1 + suma 0
     suma 0 = 0*)

(* tail recursion *)

let rec suma_tail n suma_partiala = 
  if n = 0 then suma_partiala
  else suma_tail (n - 1) (suma_partiala + n)

  let rec factorial_tail n produs_initial=
    if n=1 then produs_initial
    else factorial_tail (n-1) (produs_initial *n);;
      factorial_tail 15 1;;


let rec factorial n = 
  if n = 0 then 1 
  else n * factorial (n - 1);;

(* pattern matching *)

let rec factorial n = match n with 
  | 0 -> 1 
  | _ -> n * factorial (n - 1);;

(* 0 1 1 2 3 ..*)
(* fibo n = fibo (n-1) + fibo (n-2)*)
let rec fibo n = match n with 
  | 1 -> 0
  | 2 -> 1 
  | n -> fibo (n - 1) + fibo (n-2)

let rec fibo = function 
  | 1 -> 0
  | 2 -> 1 
  | n -> fibo (n - 1) + fibo (n-2)

(* conditii suplimentare la pattern matching *)

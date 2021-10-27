1;;
2 + 3;;
12 * 3;;
3 - 9;;
12 mod 5;; (* 12 % 5 *)
12.5 +. 39.;;

let x = 5;;
let y = 25.6;;

12.5 +. float_of_int (39 + 6);; (* float_of_int(39) *)

(* Problema 1. Fie x = -29
   Calculati, prin functii de biblioteca (predefinite): 
      a) y = modulul (valoarea absoluta) lui x
      b) z = radical de ordinul 2 (radacina patrata) din y
      c) a = partea intreaga a lui z*)

let x = -29;;
let y = abs x;;
let z = sqrt (float_of_int y);;
let a = int_of_float z;;
let b = truncate z;;
let c = int_of_float (ceil z);;

(* Problema 2. Fie x = 5, y = 10 si z = -15.
   Calculati, prin comparatii: 
      a) a = valoarea booleana daca x este mai mic decat y.
      b) b = valoarea booleana daca y este mai mic decat z.
      c) r = un mesaj sugestiv despre comparatia dintre x si z. *)

let x = 5;;
let y = 10;;
let z = -15;;
let a = x < y;;
let b = y < z;;
let c = a && b;;
let d = a || b;;

let r = if x < z && y == z then " x < z " else " x >= z "
(* C: if(cond)
    string = " x < z" 
*)
let a = 5;;

let r = if x < y then 1. else (float_of_int z);;

(* Problema 3. Definiti o functie care ia ca parametru un numar fractionar si returneaza partea intreaga. *)

let parte_intreaga x = if x >= 0. then int_of_float x else int_of_float x - 1;;

(* Problema 4. Definiti o functie cu 2 parametrii: o functie f si un numar intreg x si returneaza valoarea f(x*2) - 1. *)

let g x = x + 5;;
let funct f x = f (x * 2) - 1;;

let suma a b = a + b;;

let g = suma 2;;

let p = if x < z then 0
  else if x < y then 1
  else 2
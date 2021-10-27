(* discutie tema 2 *)

let rec suma n = 
  let rec sumaCifrelor n suma = match n with 
    | negativ when negativ < 0 ->  sumaCifrelor (- n) suma
    | 0 -> suma
    | _ -> (n mod 10) + sumaCifrelor (n / 10) suma
  in sumaCifrelor n 0;;

let rec cifrePare n = match n with 
  | neg when neg < 0 -> cifrePare (-n)
  | cifra when cifra < 10 -> 1 - (cifra mod 2)
  | par when par mod 2 = 0 -> 1 + cifrePare (n/10)
  | _ -> cifrePare (n / 10);;

let rec repr_binara n = match n with
  | 0 -> "0"
  | 1 -> "1"
  | _ -> repr_binara (n / 2) ^ (string_of_int (n mod 2));;

let rec repr_binara2 n = match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> (repr_binara2 (n / 2) * 10) + (n mod 2);;

let rec count_1_repr_binara n b = match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> count_1_repr_binara (n / b) b + (n mod b);;

(* liste *)

(* [1; 2; 3; ...; n] *)

let one_to_n n = 
  let rec one_to_n_aux n list_p = match n with 
    | 0 -> list_p
    | _ -> one_to_n_aux (n-1) (n :: list_p)
  in one_to_n_aux n [];;

(* exceptii *)

let interval a b =
  let rec interval_aux a b list_p= match b with
    | _ when a > b -> failwith "a > b! Interval invalid."
    | nr when b=(a-1) -> list_p
    | _ -> interval_aux a (b-1) (b :: list_p)
  in interval_aux a b [];;

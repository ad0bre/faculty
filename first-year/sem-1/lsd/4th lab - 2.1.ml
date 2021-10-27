(* mapare *)

let rec mapare_jum lst = match lst with
  | [] -> []
  | h :: t -> h / 2 :: (mapare_jum t)

let rec mapare_triplu lst = match lst with
  | [] -> []
  | h :: t -> h * 3 :: (mapare_triplu t)

let rec mapare f lst = match lst with
  | [] -> []
  | h :: t -> f h :: (mapare f t);;

let lst = [1; 2; 3; 5; 9; 17];;
let mapare_jum lst = List.map (fun x -> x /2) lst;;
let mapare_triplu lst = List.map (fun x -> x * 3) lst;;

(* filtrare *)

let rec filtare_pare lst = match lst with 
  | [] -> []
  | h :: t -> if h mod 2 = 0 
      then h :: filtare_pare t
      else filtare_pare t;;

let rec filtare_numere lst = match lst with (* [15; 13; 23; 14; 993] => [13; 23; 993]*)
  | [] -> []
  | h :: t -> if h mod 10 = 3 
      then h :: filtare_numere t
      else filtare_numere t;;

let rec filtare cond lst = match lst with
  | [] -> []
  | h :: t -> if cond h
      then h :: filtare cond t
      else filtare cond t;;

let l = [15; 13; 23; 14; 993];;
let r = filtare (fun x -> x mod 10 = 3) l;;
let filtare_numere lst = List.filter (fun x -> x mod 10 = 3) lst;;

let r = filtare_numere l;;

(* ex. 1 Pornind de la o lista, scrieti functia ce returneaza o lista ce contine triplul elementelor impare din lista initiala. *)

(* let l = [1; 0; (-5); 2; 7; 4; 6; 1] => [3; (-15); 21; 3] *)

let triplu_impare lst = List.map (fun x -> x * 3) (List.filter (fun x -> x mod 2 <> 0) lst);;
let triplu_impare_ineficient lst = List.filter (fun x -> x mod 2 <> 0) (List.map (fun x -> x * 3) lst);;

(* operatorul |> *)

let triplu_impare lst = lst |> List.filter (fun x -> x mod 2 <> 0) |> List.map (fun x -> x * 3);; 
let x = 100;;
let a = x |> (fun x -> x/2) |> (fun x -> x * 7);;

(* tuple *)

let pct_a = (4., 5.);;
let persoana = ("15414", "Ana", 1995, 10);;
let (id, nume, an, nota) = persoana;;

let distance a b = 
  let (xa, ya) = a in
    let (xb, yb) = b in
      sqrt ((xa -. xb) *. (xa -. xb) +. (ya -. yb) *. (ya -. yb));;

let m = (7., (-4.))
let n = (1., 13.)
let d = distance m n;;

let lst_pcte = [(9, 0); (0, 2); (1, 2); ((-5), 3); (5, 6); (0,(-7))];; (* => [(1, 2); ((-5), 3); (5, 6)] *)

(* ex. 2 Pornind de la o lista cu puncte in sistemul xOy, filtrati lista, eliminand elementele ce se afla pe axele Ox si Oy.*)

let rec filtrare_puncte lst = match lst with
  | [] -> []
  | h :: t -> let (xh, yh) = h in
      if xh = 0 || yh = 0
        then filtrare_puncte t
        else h :: filtrare_puncte t;;

let rec filtrare_puncte lst = match lst with
  | [] -> []
  | (xh, yh) :: t -> 
      if xh = 0 || yh = 0
        then filtrare_puncte t
        else (xh, yh) :: filtrare_puncte t;;

let r = filtrare_puncte lst_pcte;;

let r = List.filter (fun (x, y) -> x <> 0 && y <> 0) lst_pcte;;


(* ex. 3 Pornind de la o lista cu puncte in sistemul xOy, returnati lista cu distantele fiecarui punct pana la origine.*)



(* ex. 4 *)

let rec suma_lista lst = match lst with 
  | [] -> 0
  | h :: t -> h + suma_lista t;;

let rec prod_lista lst = match lst with 
  | [] -> 1
  | h :: t -> h * suma_lista t;;

let rec fold lst vInit op = match lst with
  | [] -> vInit
  | h :: t -> op h (fold t vInit op);;

let suma_list lst = List.fold_left (fun acc el -> acc + el ) 0 lst;;
let prod_list lst = List.fold_left (fun acc el -> acc * el) 1 lst;;

let persoane = [
  ("Ana", 2006);
  ("Andrei", 1997);
  ("Bianca", 1996);
  ("Oana", 2005);
  ("Bogdan", 2001);
  ("George", 1985)
];;

(* 1) returneaza lista tuturor persoanelor nascute dupa 2000 si varsta lor 
   [  ("Ana", 15); ("Oana", 16); ("Bogdan", 20); ];;

   2) returneaza anul nasterii al celei mai tinere persoane din lista
   2006

  *)


let pers_nasc_dupa2000 persoane =List.map ( fun (nume,an) -> (nume,2021- an)) ( List.filter (fun (nume, an) -> an >= 2000 ) persoane);;
pers_nasc_dupa2000 persoane;;
  
let an_nastere_persoana_tanara persoane = List.fold_left (fun m_partial (nume,an) -> max m_partial an ) 0 persoane ;;
an_nastere_persoana_tanara persoane;;
  
let interval persoane = (List.fold_left (fun min_partial (nume,an) -> min min_partial an ) 3000 persoane,
                         List.fold_left (fun max_partial (nume,an) -> max max_partial an ) 1900 persoane) ;;

let interval persoane = List.fold_left (fun (min_p, max_p) (nume,an) -> (min min_p an, max max_p an) ) (3000, 1900) persoane;;

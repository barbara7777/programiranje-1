(*  prva naloga  *)

let razlika_kvadratov x y = (x + y) * (x + y) - (x * x + y * y)

let uporabi_na_paru (a, b) f = (f a , f b)

let ponovi_seznam sez n =
  let rec ponovi' sez acc = function
  | n when n <= 0 -> acc
  | n -> ponovi' sez (acc @ sez) (n-1)
  in ponovi' sez [] n

let rec ponovi_seznam sez n =
  if n <= 0 then 
    [] 
  else 
    sez @ ponovi_seznam sez (n -1)

let razdeli sez = 
  let rec raz negativni preostali = function
    | [] -> (List.rev negativni, List.rev preostali)
    | x :: xs when x < 0 -> raz (x :: negativni) preostali xs
    | x :: xs -> raz negativni (x :: preostali) xs
  in raz [] [] sez


(*  druga naloga  *)
(* deli in vladaj 
najprej pogledam 11.
Imam 2 moznosti:
1) 11 je v verigi
2) 11 ni v verigi

1) spet dve moznosti 
    1.1) padajoča v desnem + 11 + naraščajoča v levem
    1.2) padajoča v levem + 11 + naraščajpča v desnem

2) spet *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

let test1 = 
  Node(
    Node(leaf 3, 10, Node(leaf 14, 13, leaf 6)),
    11,
    Node(leaf 2, 8, leaf 10)
    )

(* napišimo najprej pomožni funkciji - za padajoče in naraščajoče zaporedje *)

let rec padajoca v = function 
  | Empty -> []
  | Node (lt, x, rt) when x > v -> []
  | Node (lt, x, rt) ->
    let left = padajoca x lt in
    let right = padajoca x rt in
    if List.length left > List.length right then
      x :: left 
    else
      x :: right 


let rec narascajoca v = function 
  | Empty -> []
  | Node (lt, x, rt) when x < v -> []
  | Node (lt, x, rt) ->
    let left = narascajoca x lt in
    let right = narascajoca x rt in
    if List.length left > List.length right then
      x :: left 
    else
      x :: right



let rec  monotona_pot = function
  | Empty -> []
  | Node (lt, x, rt) ->
  (* Recursive search for  *)
    let pure_left = monotona_pot lt in
    let pure_right = monotona_pot rt in
    let left_to_right = (padajoca x lt) @ [x] @ (narascajoca x rt) in
    let right_to_left = (narascajoca x rt) @[ x] @ (padajoca x lt) in
    let options = [pure_left; pure_right; left_to_right; right_to_left] in
    let pick_bigger x y = if List.length x > List.length y then x else y in
    List.fold_left pick_bigger pure_left options


(*  trejta naloga  *)

type 'a verzija =
  | Filter of ('a -> bool) * 'a list * 'a verzija
  | Ostalo of 'a list

let test = 
  Filter ((fun x -> x > 0), [],
  Filter ((fun x -> x < 0), [],
  Ostalo []))

(* b *)

let rec vstavi x veriga =
  match veriga with 
  | Ostalo (elementi) -> Ostalo (x :: elementi)
  | Filter (f, elementi, filtri) ->
    if f x then
      Filter (f, x :: elementi, filtri)
    else
      Filter (f, elementi, vstavi x filtri)

(* c *)

let rec poisci x = function
  | Ostalo elementi -> List.mem x elementi
  | Filter (f, elementi, filtri) -> (*najprej preverimo, če je x sploh lahko notri *)
    if f x then List.mem x elementi else poisci x filtri

(* d *)

let rec izprazni = function
  | Ostalo elementi -> (Ostalo [], elementi)
  | Filter (f, elementi, filtri) -> 
    let prazni_filtri, pobrani_elementi = izprazni filtri in 
    let vsi_elementi = elementi @ pobrani_elementi in
    (Filter (f, [], prazni_filtri), vsi_elementi)

(* e *)

let rec  dodaj f veriga =
  let veriga' = Filter (f, [], veriga) in
  let prazna_veriga, elementi = izprazni veriga' in 
  List.fold_left (fun v x -> vstavi x v) prazna_veriga elementi
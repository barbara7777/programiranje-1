(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(*
type 'a tree =
  | Empty
  | Leaf 'a
  | Node of 'a tree * 'a * 'a tree

Leaf x = Node (Empty, x, Empty)
*)

let leaf x = Node (Empty, x, Empty)

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

let test_tree =
  let left_tree = Node(leaf 0, 2, Empty) in
  let right_tree = Node(leaf 6, 7, leaf 11) in 
  Node(left_tree, 5, right_tree)

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

let rec mirror tree =
  match tree with
  | Empty -> Empty
  | Node(left, x, right) -> Node(mirror right, x, mirror left)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

let rec size = function
  | Empty -> 0
  | Node (left, x, right) -> 1 + size left + size right



(*
PRIDE V POSTEV ZA IZPIT


kako bi take funkcije naredili repno rekurzivno
*)


let tl_rec_size tree =
  let rec size' acc queue =   (* v queue imamo se tista drevesa, ki jih se moramo obdelati;
  poglejmo kateri je naslednji element v vrsti za obravnavo. *)
  match queue with
  | []-> acc
  | t :: ts -> (
    (*  obravnavamo drevo *)
    match t with
    | Empty -> size' acc ts  (* Prazno drevo smo odstranili iz vrste *)
    | Node(left, x, right) ->
    let new_acc = acc + 1 in (* Obravnavamo vozlisce *)
    let new_queue = left :: right :: ts in (* Dodamo poddrevesa v vrsto *)
    size' new_acc new_queue
  )
  (* Zazenemo pomozno funkcijo *)
  in size' 0 [tree]


let rec height = function
  | Empty -> 0
  | Node (left, _, right) -> if height left < height right then 1 + height right
    else 1 + height left


(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f tree = 
  match tree with
  | Empty -> Empty
  | Node (left, x, right) -> Node (map_tree f left, f x, map_tree f right)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
  | Empty -> []
  | Node (left, x, right) -> list_of_tree left @ [x] @ list_of_tree right

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

let rec is_bst = function
  | Empty -> true
  | tree -> let sez = list_of_tree tree in 
    let rec is_sorted = function
      | [] | [_] -> true
      | x :: y :: rest -> if x > y then false 
      else is_sorted (y :: rest)
      in is_sorted sez 

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

let rec insert el = function
  | Empty -> Node(Empty, el, Empty)
  | Node(Empty, x, Empty) -> if el < x then Node(leaf el, x, Empty)
  else Node(Empty, x, leaf el)
  | Node(left, x, right) -> if el < x then
    insert el left else insert el right


let rec member el = function
  | Empty -> false
  | tree -> List.exists (fun x -> x = el) (list_of_tree tree)


(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 el = function
  | Empty -> false
  | Node(left, x, right) -> if el = x then true else
  member2 el left || member2 el right

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let succ = function
  | Empty -> None
  | Node(left, x, right) -> if (member2 (x + 1) left || member2 (x + 1) right) 
  then Some (x + 1)
  else None

let pred = function
  | Empty -> None
  | Node(left, x, right) -> if (member2 (x - 1) left || member2 (x - 1) right) 
  then Some (x - 1)
  else None

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

let rec delete el = function
  | Empty -> Empty
  | Node(left, x, right) -> if el = x then Node(left, 0, right) else 
    Node(delete el left, x, delete el right)

(* Asistentova resite *)
let rec delete x tree =
  match tree with
  | Empty -> (* Empty case *) Empty
  | Node(Empty, y, Empty) when x = y -> (* Leaf case *) Empty
  | Node(Empty, y, rt) when x = y -> (* One sided *) rt 
  | Node(lt, y, Empty) when x = y -> (* One sided *) lt 
  | Node(lt, y, rt) when x <> y -> (* Recourse deeper *)
    if x > y then
      Node(lt, y, delete x rt)
    else
      Node(delete x lt, y, rt)
  | Node(lt, y, rt) -> (* SUPER FUN CASE *) 
    match succ tree with
    | None -> failwith "HOW IS THIS POSSIBLE?" (* This can not happen *)
    | Some z -> Node(lt, z, delete z rt)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict =
  | DEmpty
  | DNode of ('key, 'value) dict * 'key * 'value * ('key, 'value) dict

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict = DNode(DNode(DEmpty, "a" , 0, DEmpty), "b" , 1, DNode(DNode(DEmpty, "c" , -2, DEmpty), "d" , 2, DEmpty))

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key = function
  | DEmpty -> None
  | DNode (left, dict_key, dict_value, right) -> 
    if key = dict_key then Some dict_value
    else if key < dict_key then dict_get key left else dict_get key right


(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 1
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_dict dict =
  let print key value = print_string (key ^ " : "); print_int value in 
  match dict with
  | DEmpty -> ()
  | DNode(DEmpty, dict_key, dict_value, DEmpty) -> 
    print dict_key dict_value
  | DNode (left, dict_key, dict_value, right) -> 
    (print_dict left;
    print dict_key dict_value;
    print_dict right)  


(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 1
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 1
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert key value = function
  | DEmpty -> DNode(DEmpty, key, value, DEmpty)
  | DNode(left, d_key, d_value, right) when key = d_key -> DNode(left, d_key, value, right) 
  | DNode (left, d_key, d_value, right) -> 
    if key > d_key then DNode (left, d_key, d_value, dict_insert key value right)
    else DNode (dict_insert key value left, d_key, d_value, right)

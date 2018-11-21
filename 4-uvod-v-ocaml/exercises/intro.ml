
(* ========== Exercise 1: Introduction to OCaml  ========== *)


(*----------------------------------------------------------------------------*]
 The function [penultimate_element] returns the second-to-last element of a
 list. If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec ultimate_element list =    (* finkcija samo za čisto zadnji element *)
   match list with
   | [] -> failwith "List too short"
   | y :: [] -> y
   | y :: ys -> ultimate_element (ys)


let rec penultimate_element list = 
  match list with 
   | [] -> failwith "List too short"
   | x :: [] -> failwith "List too short"
   | x :: y :: [] -> x
   | x :: y :: ys -> penultimate_element (y :: ys)


let rec penultimate_element = function   (*polepšana funkcija *)
  | _ :: [] | [] -> failwith "List too short"
  | x :: _ :: [] -> x
  | _ :: y :: ys -> penultimate_element (y :: ys)

let rec penultimate_element = function
  | [] | [_] -> failwith "list too short"
  | x :: [_] -> x
  | _ :: xs -> penultimate_element xs


(*----------------------------------------------------------------------------*]
 The function [get k list] returns the [k]-th element in the list [list].
 Numbering (as usual) starts with 0. If [k] is negative, the function returns
 the first element.
 If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get k list =
  match k, list with
  | _, [] -> failwith "list too short"
  | k, list when (List.length list < k) -> []
  | k, x :: xs when k <= 0 -> x
  | k, x :: xs -> get (k-1) xs

let rec get k list =
  match k, list with    	(*če naštevam vmes z vejicami, je to kot par *)
  | _, [] -> failwith "List too short"
  | k, x :: xs when k <= 0 -> x
  | k, x :: xs -> get (k - 1) xs


  (*ker v resnici matchamo samo seznam, k je neuporabljen *)
let rec get k = function
  | [] -> failwith "List too short"
  | x :: xs -> if k <= 0 then x else get (k-1) xs
  
(*----------------------------------------------------------------------------*]
 The function [double list] doubles the occurences of elements in the list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
| [] -> []
| x :: xs -> x :: x :: double xs

(*----------------------------------------------------------------------------*]
 The function [divide k list] divides the list into a pair of lists. The first
 list contains the first [k] elements of the list and the second contains the
 rest.
 When [k] is outside the bounds of [list], the appropriate list should be empty.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k list =
  match k, list with
  | _, [] -> ([], [])
  | k, list when (k <= 0) -> ([], list)
  | k, x :: xs ->
    let (list1, list2) = divide (k - 1) xs 
    in (x :: list1, list2)
      
(*----------------------------------------------------------------------------*]
 The function [delete k list] removes the [k]-th element of the list.
 If the list is too short it raises an error.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec delete k list =
  match k, list with
  | _, [] -> failwith "list too short"
  | 0, x :: xs -> xs
  | k, x :: xs -> x :: delete (k-1) xs


(*----------------------------------------------------------------------------*]
 The function [slice i k list] returns the sub-list of [list] from the [i]-th
 up to (excluding) the [k]-th element. Suppose that [i] and [k] are fitting.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let slice i k list =
  let (_, slice1) = divide i list in
  let (slice2, _) = divide (k - i) slice1 in
  slice2

(*
let rec reverse list =
  let rec reverse' acc = function
    | [] -> acc
    | x :: xs -> reverse' (x :: acc) xs
  in reverse' [] list

let rec slice i k list =
  let rec slice' i k acc list=
    match i, k, list with
    | _, _, [] -> failwith "list too short"
    | i, k, list when i >= k -> acc
    | 0, k, x :: xs -> slice' 0 (k-1) (x :: acc) xs
    | i, k, x :: xs -> slice' (i-1) (k-1) acc xs
  in slice' i k [] (reverse list)
*)

(*----------------------------------------------------------------------------*]
 The function [insert x k list] inserts (not replaces) [x] into the list at the
 index [k].
 If [k] is outside of bounds of [list], insert the element at the beggining or
 the end instead.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k list =
  let (prvi_del, drugi_del) = divide k list in
  prvi_del @ [x] @ drugi_del


(*----------------------------------------------------------------------------*]
 The function [rotate n list] rotates the list to the left by [n] places.
 Suppose that [n] is within the bounds of [list].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list =
  match n, list with
  | 0, _ | _, [] -> list
  | n, x :: xs -> rotate (n-1) (xs @ [x])

(*----------------------------------------------------------------------------*]
 The function [remove x list] removes all occurrences of [x] in the list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x list =
  match x, list with
  | _, [] -> []
  | x, y :: ys -> if x = y then remove x ys else ([y] @ remove x ys)


(*----------------------------------------------------------------------------*]
 The function [is_palindrome] checks if a list is a palindrome.
 Hint: Use an auxiliary function that reverses a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome list =
  let rec invert list acc =
    match list with
    | [] -> acc
    | x :: xs -> invert xs (x :: acc)
    in invert list [] = list
  

(*----------------------------------------------------------------------------*]
 The function [max_on_components] returns a list with the maximum element
 of the two given lists at the each index.
 The lenght of the returned list should be equal to the shorter of the lists.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components list1 list2 =
  match list1, list2 with
  | _, [] | [], _ -> []
  | x :: xs, y :: ys -> if x > y then (x :: max_on_components xs ys)
  else (y :: max_on_components xs ys)

(*----------------------------------------------------------------------------*]
 The function [second_largest] returns the second largest value in the list.
 Multiple occurrences of the same element count as one value.
 Suppose the list contains at least two distinct values.
 Hint: Use an auxiliary function that finds the maximum element of a list.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

(*)
let rec largest list = 
  let rec largest' biggest lsit =
  match list with 
  | x :: xs -> if x > biggest then largest' x xs else largest' biggest xs
  in largest' 0 list

let rec second_largest list =
  largest (remove (largest list) list)
*)

let second_largest list =
  let rec largest = function
    | [] -> failwith "List is too short."
	  | x :: [] -> x
	  | x :: xs -> max x (largest xs)
  in
  largest (delete (largest list) list)

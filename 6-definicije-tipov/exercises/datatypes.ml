(* ========== Exercise 3: Types  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 When modeling money, we usually use floats. However we quickly run into
 problems when currency is introduced.
 We shall look at two attempts to improve safety when using currency.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(* definiranje TIPOV

let x = .... definiramo neko spremenljivko
let x arg arg = .... definiramo funkcijo

type option = None | Some of int     ali nimamo ničesar, ali pa nekaj kar je int

type 'a option = None | Some of 'a

type 'a list = ....

primeri :
type color =
      | Red
      | Blue
      | Yellow
      | RGB of int * int * int

type 'a list =
      | Empty     to bi bilo definirano kot []
      | Cons of 'a * 'a list          to je x :: xs

type 'a tree =
      | Empty
      | Node of 'a * 'a tree * 'a tree

let has_zero tree =
      match tree with
      | Empty -> ....
      | Node (x ,left_tree, right_tree) -> ....
*)




(*----------------------------------------------------------------------------*]
 Define the types [euro] and [dollar], where each has only one constructor,
 which accepts a float.
 Then define the functions [euro_to_dollar] and [dollar_to_euro] which convert
 between the two currencies (get the correct factors online or make them up).

 Hint: Marvel at how informative the types are.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

type euro = Euro of float
type dollar = Dollar of float

let dollar_to_euro_bad euro = 0.2 *. euro

let dollar_to_euro_good dollar =
      match dollar with 
      |Dollar v -> Euro (0.2 *. v)

(*  drugi moznosti
let Dollar v = dolar in ...
*)

(*----------------------------------------------------------------------------*]
 Define the type [currency] as a single variant type with constructors for the
 currencies yen, pound and krona. Then define the function [to_pound], which
 converts the given currency to the pound.

 Hint: Additionally add the franc as a currency and get excited over the fact
       that Ocaml reminds you to correct the function [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency = 
  | Yen of float
  | Pound of float
  | Krona of float

let to_pound c =
  match c with
  | Yen v -> Pound (1. *. v)
  | Pound v -> Pound v
  | Krona v -> Pound (0.4 *. v)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 We wish to use lists that keep integers as well as booleans. This can be
 solved by introducing a type of integer or boolean values, however we will
 instead introduce a new type for lists.

 Recall that the type [list] uses a constructor for the empty list [Nil]
 (or [] in Ocaml) and a constructor for an element [Cons(x, xs)] (or x :: xs in
 Ocaml).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Define the type [intbool_list] with constructors for:
  1.) the empty list,
  2.) an integer element,
  3.) a boolean element.

 Define an example, which represents "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list = 
  | Empty
  | IntVal of int * intbool_list   
  | BoolVal of bool * intbool_list
  (* konstruktor lahko sprejme samo en arguzment. 
  ce hocemo, da jih vec, mu lahko podamo par elementov *)

let example = IntVal (5, (BoolVal (true, (BoolVal (false, (IntVal (7, Empty)))))))


(*----------------------------------------------------------------------------*]
 The function [intbool_map f_int f_bool ib_list] maps the values of [ib_list]
 into a new [intbool_list] using the appropriate function out of [f_int] and
 [f_bool].
[*----------------------------------------------------------------------------*)

(* kako to izgleda *)
let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs

(* 
na naš način:
let rec map_ugly f = function
  | Empty -> Empty
  | Constructor(x, xs) -> Constructor(f x, map_ugly xs)
*)

let rec intbool_map (f_int : int -> int) (f_bool : bool -> bool) = function
| Empty -> Empty
| IntVal (i, ib_list) ->
  IntVal (f_int i, intbool_map f_int f_bool ib_list)
| BoolVal (b, ib_list) ->
  BoolVal (f_bool b, intbool_map f_int f_bool ib_list)

let rec intbool_map (f_int : int -> int) (f_bool : bool -> bool) = function
| Empty -> Empty
| IntVal (i, ib_list) ->
  let new_i = f_int i in
  let new_tail = intbool_map f_int f_bool ib_list in
  IntVal (new_i, new_tail)
| BoolVal (b, ib_list) ->
  let new_b = f_bool b in
  let new_tail = intbool_map f_int f_bool ib_list in
  BoolVal (new_b, new_tail)

(*----------------------------------------------------------------------------*]
 The function [intbool_reverse] reverses the order of elements of an
 [intbool_list]. The function is tail-recursive.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse ib_list = 
  let rec reverse (acc : intbool_list) = function
  | Empty -> acc
  | IntVal (i, ib_tail) -> 
      let new_acc = IntVal (i, acc) in
      reverse new_acc ib_tail
  | BoolVal (b, ib_tail) -> 
      let new_acc = BoolVal (b, acc) in
      reverse new_acc ib_tail
  in reverse Empty ib_list


let test = BoolVal(true, IntVal(1, BoolVal (false, Empty)))

let rec intbool_reverse ib_list = 
  let rec reverse acc = function
  | Empty -> acc
  | IntVal (i, tail) -> reverse (IntVal (i, acc)) tail
  | BoolVal (b, tail) -> reverse (BoolVal (b, acc)) tail
  in reverse Empty ib_list

(*----------------------------------------------------------------------------*]
 The function [intbool_separate ib_list] separates the values of [ib_list] into
 a pair of regular [list] lists, where the first one includes all integers and
 the second one all boolean values. The function is tail-recursive and does not
 change the order of elements.
[*----------------------------------------------------------------------------*)
let rec reverse list =
  let rec reverse' acc = function
  | [] -> []
  | x :: xs -> reverse' (x :: acc) xs
in reverse' [] list

let rec intbool_separate ib_list =
  let rec separate integers booleans = function
  | Empty -> (List.rev integers, List.rev booleans)
  | IntVal (i, tail) -> separate (i :: integers) booleans tail
  | BoolVal (b, tail) -> separate integers (b :: booleans) tail
  in separate [] [] ib_list

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 You were chosen to be the database administrator for a world renowned wizard
 university "Effemef". Your task is to construct a simple system for data
 management.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Wizards are classified according to their chosen school of magic. Define a
 type [magic] which includes the magic of fire, frost and arcane.

 After being employed, a wizard can decide to be a historian, a teacher or
 a researcher. Define the type [specialisation] that represents those choices.
[*----------------------------------------------------------------------------*)

type magic = 
  | Fire
  | Frost
  | Arcane

type specialisation = 
  | Historian
  | Teacher
  | Researcher

(*----------------------------------------------------------------------------*]
 Every wizard starts out as a newbie. Afterwards they become a student and in
 the end, they may get employed. Define the type [status] which determines if a
 wizard is:
  a.) a newbie,
  b.) a student (also what school of magic they study and for how long),
  c.) an employee (also their school of magic and specialisation).

 Then define a record type [wizard] which has a field for the wizards name and
 a field for their status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)

type status =
  | Newbie
  | Student of magic * int
  | Employee of magic * specialisation 

type wizard = {name : string; status : status}

(*----------------------------------------------------------------------------*]
 We want to count how many users of a certain school of magic are currently in
 the group.
 Define a record type [magic_counter] which has an integer field for every
 school of magic.
 Then define the function [update counter magic] that returns a new counter
 with an updated field depending on the value of [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {fire : int; frost : int; arcane : int}

let update counter = function
  | Fire -> {counter with fire = counter.fire + 1}
  | Frost -> {counter with frost = counter.frost + 1}
  | Arcane -> {counter with arcane = counter.arcane + 1}

(*----------------------------------------------------------------------------*]
 The function [count_magic] accepts a list of wizards and counts the users of
 different schools of magic.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let rec count_magic = 
  let rec count  counter = function
  | [] -> counter
  | x :: xs -> count (update counter x) xs
  in count {fire = 0; frost = 0; arcane = 0} 

let count_magic wizard_list =
  let rec count counter = function
    | [] -> counter
    | {name; status} :: wizards -> (
        match status with
        | Newbie -> count counter wizards
        | Student (magic, _) -> count (update counter magic) wizards
        | Employee (magic, _) -> count (update counter magic) wizards)
  in count {fire = 0; frost = 0; arcane = 0} wizard_list


(*----------------------------------------------------------------------------*]
 We wish to find a possible candidate for a job offer. A student can become a
 historian after studying for at least 3 years, a researcher after 4 years and
 a teacher after 5 years.
 The function [find_candidate magic specialisation wizard_list] searches
 through the list of wizards and returns the name of a suitable candidate for
 the [specialisation] if they are studying [magic]. If there is no candidate,
 it should return [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate magic specialisation wizard_list =
  match wizard_list with
  | [] -> None
  | {name; status} :: wizards -> 
    (match status with
    | Newbie -> find_candidate magic specialisation wizards
    | Employee (_, _) ->  find_candidate magic specialisation wizards
    | Student (learn_magic, years) -> if  (learn_magic = magic) then
        (match specialisation with
        | Historian -> if (years >= 3) then Some name else find_candidate magic specialisation wizards
        | Teacher -> if years >= 4 then Some name else find_candidate magic specialisation wizards
        | Researcher -> if years >= 5 then Some name else find_candidate magic specialisation wizards)
        else find_candidate magic specialisation wizards)
  
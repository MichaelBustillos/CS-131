(* rec is for recursively defined function 
   match matches the parameter with every value after | and executes the expression after
*)
type ('a, 'b) symbol = N of 'a | T of 'b

(*
 head points to first element while tail is the tail or rest of the list of a
 val exists : ('a -> bool) -> 'a list -> bool
 exists p [a1; ...; an] checks if at least one element of the list satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an).
 The exists function will return true if any element in the list satisfies the predicate (the function). In this case, the predicate is fun x -> not x which will return true if x is false.
*)
let rec subset a b = match a with
| [] -> true
| head::tail -> if List.exists (fun x -> head = x) b then subset tail b else false;;

(* List takes a parameter(or comparison) that compares each value of b with head and either continues recursion with next element or returns false *)


(* linear algebra definition of set equality: a is subset of b and b is subset of a *)
let equal_sets a b = (subset a b) && (subset b a)

(* Returns list containing the union of two lists a and b *)
let rec set_union a b = match a with 
| [] -> b
| head::tail -> if List.mem head b then set_union tail b else set_union tail b@[head];;

(* Returns intersection of two lists a and b *)
let rec set_intersection a b = match a with 
| [] -> []
| head::tail -> if (List.mem head b) then set_intersection tail b@[head] else set_intersection tail b;;

(*returns all elements of a that are not in b *)
let rec set_diff a b = match a with
| [] -> []
| head::tail -> let diff = set_diff tail b in if (List.mem head b) then diff else head::(diff);;

(* assuming that eq is the equality predicate for f's domain. 
A common case is that eq will be (=), that is, the builtin equality predicate of OCaml*)

let rec computed_fixed_point eq f x = let output = (f x) in
if eq output x then x else computed_fixed_point eq f output

(* if the output of the function is equal to the input, we have the fixed point, otherwise plug in the next level of function
until eventually we get a result *)


(*check if written it has a terminal*)
let isTerminal str t =
match str with
| T _ -> true
| N s -> List.mem t s;;

(* Pervasives.fst -> returns first component of pair
   Pervasives.snd -> returns the 2nd component of pair
 *)

(*extract all the second pairs of the tuples in the list. Example [(Expr, "N Expr")] would extract "N Expr" *)
let rec extract g = match g with
|[] -> []
|head::tail -> [Pervasives.snd head]@extract tail;;

(* filter p l returns all the elements of the list l that satisfy the predicate p. N a specifically matches
   all non Terminal tuple elements  *)
let getAllNodes lst g =
  List.filter (fun (a, _) -> List.mem (N a) lst) g;;

(* let findNextNodes lst g = List.flatten(extract (List.filter (fun (a,_) -> List.mem a lst) g));; *)

(* flatten a list of list of tuples  to a list of strings *)
let findNextNodes lst = List.flatten(extract(lst));;

(* let grabDiff lst g = set_diff g List.filter (fun (a,_) -> List.mem a lst) g*)

let rec recurse lst g = let f_List = (getAllNodes lst g) in match f_List with
|[] -> g
|a -> (recurse (findNextNodes a) (set_diff g a))

let rec filter_reachable g = 
(Pervasives.fst g),(set_diff (Pervasives.snd g) (recurse [N (Pervasives.fst g)] (Pervasives.snd g)));;

(*search through the list for Pervasives.fst g *)
(* use filter_reachable as the base function that feeds into a recursion *)

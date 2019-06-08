
type ('a, 'b) symbol = N of 'a | T of 'b

let rec subset a b = match a with
| [] -> true
| head::tail -> if List.exists (fun x -> head = x) b then subset tail b else false;;

let rec set_union a b = match a with 
| [] -> b
| head::tail -> if List.mem head b then set_union tail b else set_union tail b@[head];;

let rec set_intersection a b = match a with 
| [] -> []
| head::tail -> if (List.mem head b) then set_intersection tail b@[head] else set_intersection tail b;;

let rec set_diff a b = match a with
| [] -> []
| head::tail -> let diff = set_diff tail b in if (List.mem head b) then diff else head::(diff);;

let rec computed_fixed_point eq f x = let output = (f x) in
if eq output x then x else computed_fixed_point eq f output

let isTerminal str t =
match str with
| T _ -> true
| N s -> List.mem t s;;


let rec extract g = match g with
|[] -> []
|head::tail -> [Pervasives.snd head]@extract tail;;

let getAllNodes lst g =
  List.filter (fun (a, _) -> List.mem (N a) lst) g;;

let findNextNodes lst = List.flatten(extract(lst));;


let rec recurse lst g = let f_List = (getAllNodes lst g) in match f_List with
|[] -> g
|a -> (recurse (findNextNodes a) (set_diff g a))

let rec filter_reachable g = 
(Pervasives.fst g),(set_diff (Pervasives.snd g) (recurse [N (Pervasives.fst g)] (Pervasives.snd g)));;


let rec make_or_matchers gram accessible_nodes accept frag = match accessible_nodes with
    | [] -> None 
    | head::tail -> 
        let output = make_appended_matchers gram head accept frag in match output with
            | None -> make_or_matchers gram tail accept frag
            | _ -> output
and make_appended_matchers gram list accept frag = match list with
| [] -> accept frag
| _ -> match frag with
  | [] -> None
  | head::tail -> match list with
    | [] -> None
    | T expr ::nodes -> if head = expr then (make_appended_matchers gram nodes accept tail) else None
    | N expr2 ::nodes -> (make_or_matchers gram (gram expr2) (make_appended_matchers gram nodes accept) frag)

let rec make_matcher gram1 = let accessible_nodes= ((Pervasives.snd gram1) (Pervasives.fst gram1)) in 
(fun accept frag -> make_or_matchers (Pervasives.snd gram1) accessible_nodes accept frag)


let accept_empty_list x = match x with
| [] -> Some [] 
| x -> None;;

let preorder gram frag=
let rec make_or_parsers gram accessible_nodes accept frag = match accessible_nodes with
    | [] -> None 
    | head::tail -> 
        let output = make_appended_parsers gram head accept frag in match output with
            | None -> make_or_parsers gram tail accept frag
            | Some tree -> Some (head::tree)
and make_appended_parsers gram list accept frag = match list with
| [] -> accept frag
| _ -> match frag with
  | [] -> None
  | head::tail -> match list with
    | [] -> None
    | T expr::nodes -> if head = expr then (make_appended_parsers gram nodes accept tail) else None
    | N expr2::nodes -> (make_or_parsers gram (gram expr2) (make_appended_parsers gram nodes accept) frag)
in make_or_parsers (Pervasives.snd gram) ((Pervasives.snd gram) (Pervasives.fst gram)) accept_empty_list frag

let make_parser gram = 
(let rec rec_horizontal first_node pre = match (first_node) with
   | [] -> ([], pre) 
| head::tail -> (match (rec_vertical head pre) with 
             | (branch, list_left) -> (match (rec_horizontal tail list_left) with
| (ptree_rest, list_left2) -> (branch::ptree_rest, list_left2)))
and rec_vertical first_node pre = match (first_node) with
| T expr -> (Leaf expr, pre)
| N expr2 -> (match pre with
          | [] -> (Node (expr2, []), [])
| head::tail -> (match (rec_horizontal head tail) with 
             | (ptree_rest, list_left) -> (Node(expr2, ptree_rest), list_left))) in
(function frag -> match (preorder gram frag) with
      | None -> None
| Some [] -> None
| Some preorder_list -> let first_node = [N (Pervasives.fst gram)] in
(match (rec_horizontal first_node preorder_list) with
     | (a, b) -> (match a with head::tail -> Some(head)
       | _ -> None))))


type my_nonterminals = 
| Sentence | NPhrase | VPhrase | Noun | Verb | Adverb| PPhrase | Prep |Adjective| Gerund | Article;;

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x;;


type my_nonterminals = 
| Noun | Verb | Adjective | Adverb

let grammar = [
 Noun, [N Noun; N Verb; N Adverb];
 Noun, [N Verb];
 Noun, [N Adjective];
 Verb, [N Adverb];
 Adjective, [T"likely"];
 Adjective, [T"extraordinary"];
 Adverb, [T"quick"]]

let test_awkish_grammar =
  (Sentence,
   function
     | Sentence ->
         [[N NPhrase; N VPhrase]]
     | NPhrase ->
      [[N Article; N Adjective; N Noun]; [T"Kevin's"; N Gerund]; [N Noun]]
     | VPhrase ->
[[N Adverb; N Verb; N NPhrase]; [N Verb; N PPhrase]; [N Verb]]
     | Noun ->
      [[T"dragon"]; [T"fire"]; [T"water"]; [T"light"]; [T"demon"]]
     | Verb ->
 [[T"drank"]; [T"laughed"]; [T"burned"]; [T"drank"]; [T"talking"]]
     | Gerund ->
     [[T"laughing"]; [T"crying"]]
     | Adjective ->
 [[T"scary"]; [T"extraordinary"]; [T"languid"]]
     | Adverb ->
      [[T"quickly"]; [T"surprisingly"]]
     | Article ->
[[T"the"]]
     | PPhrase ->
      [[N Prep; N NPhrase]]
     | Prep ->
 [[T"with"]; [T"about"]; [T"through"]]);;

let test_frag = ["Kevin's"; "laughing"; "surprisingly"; "burned"; "the"; "languid"; "dragon"];;

let make_matcher_test = ((make_matcher test_awkish_grammar accept_empty_suffix test_frag) = Some [])

let make_parser_test = (match (make_parser test_awkish_grammar test_frag) with 
							      | Some tree -> parse_tree_leaves tree = test_frag
    | _ -> false);;





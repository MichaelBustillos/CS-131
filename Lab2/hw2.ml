(* convert_grammar:
   input: grammar1 is non-terminal symbol, list of rules
   output: grammar 2 is non-terminal symbol and a production function *)
(* input type definitions to prevent undefined errors *)
type ('nonterminal, 'terminal) parse_tree = Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list | Leaf of 'terminal;;
type ('nonterminal, 'terminal) symbol = | N of 'nonterminal | T of 'terminal;;
type awksub_nonterminals = | Expr | Term | Lvalue | Incrop | Binop | Num;;

(* production function that takes all the symbols and recurses through them from head to tail and appends *)
let rec production_function symbol rules = match rules with
| [] -> []
| head::tail -> if (Pervasives.fst head) = symbol then [Pervasives.snd head]@production_function symbol tail else production_function symbol tail

let rec convert_grammar gram = ((Pervasives.fst gram), (function symbol -> (production_function symbol (Pervasives.snd gram))))

(* horizontal recursion, merely adds all of the vertical tree results together in an appended list *)
let rec horizontal = function 
| [] -> [] 
| head::tail -> (vertical head)@(horizontal tail) 
and vertical = function (Leaf term) -> [term] | Node (_, nonterm) -> horizontal nonterm;;

(* vertical goes down until it finds a Leaf node which it returns in a list format *)
let parse_tree_leaves tree = match tree with 
| (Leaf term) -> [term] 
| Node (_,nonterm) -> horizontal nonterm;;   

(* parse_tree_leaves essentially checks the tree and then returns the build list *)

(* make_or_matcher goes through a list of lists for each of the expressions it sees to see if 
   any of the branches that it goes down returns a successful match, any one of them 
   can work hence the oring of many different branches of parse trees
   make_and_matcher checks one specific rule and since we need all of the parts to work, we 
   need to and them together or check if all of them work. If any don't we return none.
   
   The matching works straightforwardly by checking against Terminals or Non-Terminals
   and either returning whether the terminal is matched or continues recursing
   horizontally
   *)
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


(* preorder goes through the tree and recurses down in a depth first search fashion. 
   at every tree, we get the frag and append it to a list to result in a preorder
   list traversal with a list of lists
*)
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

(*using the preorder traversal, we make a breadth first search traversal of the list and add nodes into
  our parse tree via the constructor for leaf*)
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



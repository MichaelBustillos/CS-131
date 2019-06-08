
type my_nonterminals = 
| Sentence | NPhrase | VPhrase | Noun | Verb | Adverb| PPhrase | Prep |Adjective| Gerund | Article;;

let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x;;

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

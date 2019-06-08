let my_subset_test0 = subset [] []
let my_subset_test1 = not (subset [1] [])
let my_subset_test3 = subset [1;2;3;4] [1;2;3;4]

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not(equal_sets [1] [])
let my_equal_sets_test2 = equal_sets [1;2;5] [1;2;5]

let my_set_union_test0 = equal_sets(set_union [] [1]) [1]
let my_set_union_test1 = equal_sets(set_union [1] []) [1]
let my_set_union_test2 = equal_sets(set_union [1;2;3;4] [5;6;7]) [1;2;3;4;5;6;7]

let my_set_intersection_test0 = equal_sets(set_intersection [] []) []
let my_set_intersection_test1 = equal_sets(set_intersection [1;2;3;5] [1;2;3;4]) [1;2;3]
let my_set_intersection_test2 = equal_sets(set_intersection [1; 1; 1] [3; 1; 3]) [1]

let my_set_diff_test0 = equal_sets(set_diff [1;2] [1;2]) []
let my_set_diff_test1 = equal_sets(set_diff [1;2;3] []) [1;2;3]
let my_set_diff_test2 = equal_sets(set_diff [1;5;9] [1]) [5;9]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> 6 *x / 7 ) 100000 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> sqrt x) 10000. = 1.

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
let test_grammar = Adjective, grammar
let test_grammarN = Noun, grammar
let my_filter_reachable_test0 = filter_reachable test_grammar = (Adjective, [(Adjective, [T "likely"]); (Adjective, [T "extraordinary"])])

let my_filter_reachable_test1 = filter_reachable test_grammarN = 
(Noun, 
 [(Noun, [N Noun; N Verb; N Adverb]); (Noun, [N Verb]);
  (Noun, [N Adjective]); (Verb, [N Adverb]); (Adjective, [T "likely"]);
  (Adjective, [T "extraordinary"]); (Adverb, [T "quick"])])

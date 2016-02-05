:- module(predicate_map, _).
/*
based on linearSolveProg.pl interpreter
*/

:- dynamic pred_map/2.

:- use_module(library(lists)).
:- use_module(library(dynamic)).
:- use_module(library(read)).
:- use_module(load_simple).
:- use_module(library(process), [process_call/3]).


go:-
    print_predicateMap('../mc91.lin.pl', '../mc91.parsed.pl').

/*produces a map between the predicates of the original program and the Logen paritally evaluated program,
the logen partially evaluated program has comments, which maps shows the map between the original program predicates and the logen predicates

*/
print_predicateMap(F, F1):-
    cleanup,
    load_file(F),
    parse_comments(F, F1),
    cleanup,
    load_file(F1),
    predicateMap.
    %printMap.

 /*
sed -n 's/^\/\*  \(.*\)\. \*\//\1./p' $1 > logen_renames.pl
in prolog '\' has to be replaced by '\\'

*/


parse_comments(F, F1):-
    process_call(path('sed'), ['-n', 's/^\\/\\*  \\(.*\\)\\. \\*\\//\\1./p', F],
	             [stdout(file(F1))]).


/*
map pred_map(Original_predicates, PE_predicates)

search for clauses whose:
     -body constains at most one non-constraint atom
    - the head takes 3 arguments
    - the first argument is a single element list


*/
predicateMap:-
    %look for only one atom in the body
    my_clause(H, [B], _),
    H=..[_|[[A1], _, _]],
    functor(A1, P1,N1),
    functor(B, P,N),
    assert(pred_map(P1/N1, P/N)),
    fail.
predicateMap.

printMap:-
    pred_map(O,PE),
    write(O), write(' -- '), write(PE), nl,
    fail.
printMap.

cleanup:-
    retractall(pred_map(_,_)).





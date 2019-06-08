count(_, [], Prev, Prev).
count( Height, [H|T], Prev, Num) :-
    H #> Height,                                                   
    New is Prev+1,
    count(H, T, New, Num).
count(Height, [H|T], C, Num) :-
    H #< Height,                                                   
    count(Height, T, C, Num).
count([H | T], Num) :-
    count(H, T, 1, Num).
checkRows([], []).
checkRows([BH | BT], [NH | NT]) :-
    count(BH, NH),
    checkRows(BT, NT).

restrict(N, L) :-
    length(L, N),
	fd_all_different(L),
    fd_domain(L, 1, N).

restrict_length(N, L) :-
    length(L, N).
tower(N, T, C) :-
    C = counts(Top, Bot, Left, Right),
    length(T, N),
	length(TT, N),
    maplist(restrict(N), [H | T]),
    transpose(T, TT),
    maplist(restrict(N), [H | TT]),
    maplist(fd_labeling, T),
    checkRows(T, Left),
    maplist(reverse, T, TR),
    checkRows(TR, Right),
    checkRows(TT, Top),
    maplist(reverse, TT, TTR),
    checkRows(TTR, Bot).


% transpose functions => using online transpose from SWI-prolog https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).	

label(N, L) :-
	findall(Num, between(1, N, Num), X), 
	permutation(X, L).

plain_tower(N, T, C) :-
    C = counts(Top, Bot, Left, Right),
    length(T, N),
	length(Left, N),
    length(Right, N),
    length(Top, N),
    length(Bottom, N),
    check(N, T, Left, Right),
    transpose(T, TT),
    check(N, TT, Top, Bot).

check(N, [], _, _).

check(N, [Row|T], [H1|T1], [H2|T2]) :-
    row(N, H),!,
    permutation(H, Row),
	plain_restrict(H, H1, Row),
    reverse(Row, RRow),
	plain_restrict(H, H2, RRow),
    check(N, T, T1, T2).

plain_restrict(H, H1, Row) :-
	member(H1, H),
    plain_count(0, Row, H1).

bet(N, M, K) :- N =< M, K = N.
bet(N, M, K) :- N < M, N1 is N+1, bet(N1, M, K).

row(Count, List) :-
    findall(N, between(1,Count,N), List).

plain_count(Height, [H|T], Num) :-
    H > Height,
    New is Num-1,
    plain_count(H, T, New).

plain_count(Height, [H|T], Num) :-
    H < Height,
    plain_count(Height, T, Num).
plain_count(Height, [T], 1) :-
    T > Height.

plain_count(Height, [T], 0) :-
    T < Height.

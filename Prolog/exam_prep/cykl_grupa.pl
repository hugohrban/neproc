%% indexovanie do matice
ij(M, I, J, H) :-
    i(M, I, Line),
    i(Line, J, H).

% indexovanie do pola
i([H|_], 0, H).
i([_|Rest], Ix, L) :-
    Ixx is Ix - 1,
    i(Rest, Ixx, L).

% index prvku v poli
ixof([H|_], H, 0).
ixof([_|Rest], E, N) :-
    ixof(Rest, E, N1),
    N is N1 + 1.

% vykonat grupovu operaciu A*B
oper([ELine|Rest], A, B, R) :-
    ixof(ELine, A, I),
    ixof(ELine, B, J),
    ij([ELine|Rest], I, J, R).


% exponent A^N = A*(A^N-1), kde A^1 = A
pow([[E|_]|_], _, 0, E).
pow(_, A, 1, A).
pow(Grupa, A, N, R) :-
    N1 is N - 1,
    pow(Grupa, A, N1, R1),
    oper(Grupa, R1, A, R).

% vsetky exponenty A^1, ..., A^N, pre nejake N
getPows(_, A, 1, [A]).
getPows(Group, A, N, [P|Pws]) :-
    N1 is N - 1,
    getPows(Group, A, N1, Pws),
    pow(Group, A, N, P).

% vsetky exponenty A^1, ..., A^N, kde n je rad grupy
getPows(Group, A, Pows) :-
    length(Group, Size),
    getPows(Group, A, Size, Pows), !.

% ak { G^1, ..., G^N } == { prvky grupy }, tak G je generator
cyklicka([Gr|Rest], Gen) :- 
    member(Gen, Gr),
    getPows([Gr|Rest], Gen, Pows),
    sort(Pows,PowsUnique),
    same_length([Gr|Rest], PowsUnique).
    
    


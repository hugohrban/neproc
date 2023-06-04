
%%%%%%%%%%%%%%%% 1. relacni db

concat(A, B) :- foldl(append, A, [], B).

key(K-_, K).
keys(DB, Keys) :- concat(DB, Raw), maplist(key, Raw, Dup), sort(Dup, Keys).

mkEmpty(K, K-[]).
empty(Keys, R) :- maplist(mkEmpty, Keys, R).

extractKey(Row, K, K-[Res]) :-
    ( member(K-V, Row) -> Res = V
    ; Res = undef
    ).
extractRow(Keys, Row, Res) :-
    maplist(extractKey(Row), Keys, Res).

mergeAttrs(K-L, K-R, K-Res) :- append(L, R, Dup), sort(Dup, Res).
merge(Left, Right, Res) :- maplist(mergeAttrs, Left, Right, Res).

extrakce(DB, Attrs) :-
    keys(DB, Keys),
    maplist(extractRow(Keys), DB, Raw),
    empty(Keys, Init),
    foldl(merge, Raw, Init, Attrs).




%%%%%%%%%%%% 2. alokace pameti

add(Size, Address, [], [Address-Size], Address).
add(Size, Address, [A-S|Rest], Result, FinalAddr) :-
    End is A + S,
    ( Address + Size > A -> add(Size, End, Rest, R, FinalAddr), Result = [A-S|R]
    ; Result = [Address-Size,A-S|Rest], FinalAddr = Address
    ).

addAll([], CurrentAlloc, [], CurrentAlloc).
addAll([Req|Reqs], CurrentAlloc, [Req-R|Rest], NewAlloc) :-
    add(Req, 0, CurrentAlloc, Added, R),
    addAll(Reqs, Added, Rest, NewAlloc).

mergeAlloc([], []).
mergeAlloc([A], [A]).
mergeAlloc([A1-S1,A2-S2|R], Merged) :-
    ( A2 is A1 + S1 -> S is S1 + S2, mergeAlloc([A1-S|R], Merged)
    ; mergeAlloc([A2-S2|R], M), Merged = [A1-S1|M]
    ).

alokace(Req, CurrentAlloc, Result, NewAlloc) :-
    addAll(Req, CurrentAlloc, Result, Unmerged),
    mergeAlloc(Unmerged, NewAlloc).


%%%%%%%%%%%%%%%% 3. pruchod DAG
% [a-[b,c,d], b-[c,d], c-[d]] -> tj. vrchol-[kam_idu_sipky_z_vrcholu]
pruchod(G, V, R) :-
    dfs(V, G, R, 1, _).

dfsList([], G, G, A, A).
dfsList([N|Ns], G1, G3, A1, A3) :-
    dfs(N, G1, G2, A1, A2),
    dfsList(Ns, G2, G3, A2, A3).

dfs(V, GIn, GOut, A1, A4) :-
    ( select(V-Ns, GIn, Rest) ->
        NewG = [stamp(V, In, Out)-Ns|Rest],
        In = A1,
        A2 is A1 + 1,
        dfsList(Ns, NewG, GOut, A2, A3),
        Out = A3,
        A4 is A3 + 1
    ; GOut = GIn, A4 = A1
    ).




%%%%%%%% 4. generovanie bin stromov


bin(0, nil).
bin(N, t(L, R)) :-
    N1 is N - 1,
    between(0, N1, LCount),
    bin(LCount, L),
    RCount is N1 - LCount,
    bin(RCount, R).

bin(T) :- between(0, inf, N), bin(N, T).


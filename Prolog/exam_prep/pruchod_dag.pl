% [a-[b,c], b-[c], c-[]] -> vrchol-[kam_idu_sipky_z_vrcholu]

%% G - graf (DAG), V - pociatocny vrchol, R -stamped graf
pruchod(G,V,R) :-
    dfs(V,G,R,1,_).
    %sort(R1, R).


splitBy(E,S,L,R) :-
    member(E,S),
    append([L,[E],R],S).


dfsList([],G,G,T,T).
dfsList([V|VS],GIn,GOut,TIn,TOut) :-
    dfs(V,GIn,GTemp,TIn,TTemp),
    dfsList(VS, GTemp, GOut, TTemp, TOut).


dfs(V,GIn,GOut,TIn,TOut) :-
    ( splitBy(V-NS, GIn, RestL, RestR) ->
        append([RestL, [V/TIn/Out-NS|RestR]],Labeled),
        T2 is TIn + 1,
        dfsList(NS, Labeled, GOut, T2, Out),
        TOut is Out + 1
    ; 
        GOut = GIn,
        TOut = TIn
    ).


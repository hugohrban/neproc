% Binární strom lze v Prologu reprezentovat termem b(L, V, P), který obsahuje hodnotu kořene V, levý podstrom L a pravý podstrom P. Prázdný strom lze reprezentovat atomem nil.

% a) Sestavte predikát vyššího řádu maptree/2, který aplikuje zadaný predikát P na vrcholy binárního stromu:

% maptree(+P, ?T) :- uspěje, pokud volání (unárního) predikátu P na argument V uspěje pro každý vrchol V stromu T.
% Nápověda: Inspirujte se definicí predikátu maplist na přednášce. Zejména můžete využít vestavěný predikát call, který aplikuje zadaný predikát P na zadaný argument.

% b) Sestavte predikát size(-T, +N, +H), který postupně vrátí všechny binární stromy T o N vrcholech a výšce H. Výška stromu je definována jako délka nejdelší cesty (měřená počtem hran) z kořene do listu. Např. strom b(nil, 10, t(nil, 15, nil)) má výšku 1. U prázdného stromu budeme předpokládat výšku -1.

% Predikát by měl vygenerovat všechny binární stromy se zadaným počtem vrcholů N ≥ 0 a zadanou výškou H ≥ -1. Ve vrcholech generovaných stromů budou volné proměnné, které můžeme navázat na konkrétní hodnotu predikátem maptree, jak je uvedeno v příkladech níže.

% ?- size(T, 3, 2), maptree(=(1), T).
% T = b(nil, 1, b(nil, 1, b(nil, 1, nil))) ;
% T = b(nil, 1, b(b(nil, 1, nil), 1, nil)) ;
% T = b(b(nil, 1, b(nil, 1, nil)), 1, nil) ;
% T = b(b(b(nil, 1, nil), 1, nil), 1, nil)

% ?- size(T, 1, 1).
% false

% ?- size(T, 2, 1), maptree(=(0), T).
% T = b(nil, 0, b(nil, 0, nil)) ;
% T = b(b(nil, 0, nil), 0, nil) 

% ?- size(T, 6, 2), maptree(=(2), T).
% T = b(b(nil, 2, b(nil, 2, nil)), 2, b(b(nil, 2, nil), 2, b(nil, 2, nil))) ;
% T = b(b(b(nil, 2, nil), 2, nil), 2, b(b(nil, 2, nil), 2, b(nil, 2, nil))) ;
% T = b(b(b(nil, 2, nil), 2, b(nil, 2, nil)), 2, b(nil, 2, b(nil, 2, nil))) ;
% T = b(b(b(nil, 2, nil), 2, b(nil, 2, nil)), 2, b(b(nil, 2, nil), 2, nil)) 

% Nápověda: Zvažte využití knihovního predikátu between/3.

x(0).
x(1).

y(A) :- between(0,inf,A).

maptree(_,nil).
maptree(P,b(L,V,R)) :-
    maptree(P,L),
    call(P,V),
    maptree(P,R).

depth(nil,-1).
depth(b(L,_,R),H) :-
    H1 is H - 1,
    between(-1,H1,HL),
    between(-1,H1,HR),
    depth(L,HL),
    depth(R,HR),
    H is max(HL,HR) + 1.

size(nil, 0).
size(b(L,_,R),S) :-
    S1 is S - 1,
    between(0,S1,SL),
    between(0,S1,SR),
    size(L,SL),
    size(R,SR),
    S is SL + SR + 1.

size(nil,0,-1).
size(T, N, H) :-
    depth(T,H),
    size(T,N).







% 2. domácí úloha
%
% a) Implementujte predikát flat(+List, ?Result), který zploští libovolně
% zanořený seznam seznamů List.
%
% flat([], R).
% R = [].
%
% flat([[]], R).
% R = [].
%
% flat([a,b,c], R).
% R = [a,b,c].
%
% flat([a,[[],b,[]],[c,[d]]], R).
% R = [a,b,c,d].
%
% Tento predikát měl být deterministický (speciálně otestujte, že po odmítnutí
% neprodukuje duplikátní/nesprávné výsledky). Pokuste se o efektivní
% implementaci pomocí akumulátoru.
%
% b) Implementuje predikát transp(+M, ?R), který transponuje matici M (uloženou
% jako seznam seznamů). Pokud M není ve správném formátu (např. řádky mají
% různé délky), dotaz transp(M, R) by měl selhat.
%
% transp([], R).
% R = [].
%
% transp([[],[],[]], R).
% R = [].
%
% transp([[a,b],[c,d],[e,f]], R).
% R = [[a,c,e],[b,d,f]].
%
% transp([[a],[b,c],[d]], R).
% false.
%
% c) (BONUSOVÁ ÚLOHA) Implementuje vkládání prvku pro AVL stromy.
%
% Použijte následující reprezentaci:
% prázdný strom: nil
% uzel: t(B,L,X,R) kde
%   L je levý podstrom,
%   X je uložený prvek,
%   R je pravý podstrom,
%   B je informace o vyvážení:
%     B = l (levý podstrom je o 1 hlubší)
%     B = 0 (oba podstromy jsou stejně hluboké)
%     B = r (pravý podstrom je o 1 hlubší)
%
% avlInsert(+X, +T, -R)
% X je vkládané číslo, T je strom před přidáním, R je strom po přidání
%
% avlInsert(1, nil, R).
% R = t(0, nil, 1, nil).
%
% avlInsert(2, t(0, nil, 1, nil), R).
% R = t(r, nil, 1, t(0, nil, 2, nil)).
%
% avlInsert(1, t(0, nil, 1, nil), R).
% R = t(0, nil, 1, nil).




%%%%%%%%%%%%%%%%%%%%%%% uloha a) %%%%%%%%%%%%%%%%%%%%%%%

% flat(+L, ?Acc, ?R) - flattens list L into list R using accumulator Acc
% flat(+L, ?R) - flattens list L into list R starting with empty accumulator

flat([], Acc, Acc) :- !.

flat([X|Y], Acc, Res) :-
    flat(Y, Acc, Temp),
    flat(X, Temp, Res), !.

flat(X, Acc, [X|Acc]).

flat(X, R) :-
    flat(X, [], R).




%%%%%%%%%%%%%%%%%%%%%%% uloha b) %%%%%%%%%%%%%%%%%%%%%%%



% matrix(+L)

matrix([H1|[H2|T]]) :-
    same_length(H1,H2),
    matrix([H2|T]), 
    !.

matrix([_]).


% f_(+Matrix, ?Rest, ?FirstColumn)

f_(X, [], []) :-
    flat(X, []).

f_([[H|F]|Y], [F|P], [H|R1]) :-
    f_(Y, P, R1).

% transp(+Matrix, ?Transposed)

transp(X, []) :- 
    flat(X, []), !.

transp(X, [F|R]) :-
    matrix(X),
    f_(X, Rest, F),
    transp(Rest, R).


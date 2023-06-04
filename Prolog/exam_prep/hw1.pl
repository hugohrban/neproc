% 1. domácí úloha
%
% a) Implementujte logaritmus o základu 2 (dolní celou část) na unárně
% reprezentovaných číslech.
%
% logtwo(+N, ?Vysledek)
%
% Nápověda: Může se vám hodit pomocný predikát pro půlení.
%
% logtwo(0, R).
% false.
%
% logtwo(s(s(s(0))), R).
% R = s(0).
%
% logtwo(s(s(s(s(0)))), R).
% R = s(s(0)).
%
% b) Implementujte predikát, který spočte n-té Fibonacciho číslo lépe než
% v exponenciálním čase (ideálně pouze lineárně mnoho sčítání).
%
% fib(+N, ?Vysledek)
%
% Nápověda: Zkuste nejdřív implementovat obecnější predikát, kde si můžete
% zvolit počáteční čísla.
%
% F_0 = 4
% F_1 = 5
% F_2 = 4 + 5 = 9
% F_3 = 5 + 9 = 14
%
% generalizedFib(3, 4, 5, R).
% R = 14.
%
%
% c) (BONUSOVÁ ÚLOHA) Implementuje predikát pro sčítání dvou binárních čísel.
%
% Můžete použít např. následující reprezentaci:
%
% 13[dec] = 1101[bin] = b(1, b(0, b(1, b(1, e))))
%
% Příklad použití:
% addBin(b(1, b(0, b(1, e))), b(1, b(1, b(0, b(1, e)))), R).
% R = b(0, b(0, b(0, b(0, b(1, e))))).
%
% resp.
%
% addBin([1, 0, 1], [1, 1, 0, 1], R).
% R = [0, 0, 0, 0, 1].


% half(N, N*2)
half(0,0).
half(s(0),0).
half(s(Y),s(s(X))) :- half(Y,X).

% x < y
less(0,s(_)).
less(s(X),s(Y)) :- less(X,Y).

logtwo(s(0),0).
logtwo(Y,s(R)) :- less(X,Y), half(X,Y), logtwo(X,R).



generalizedFib(A,_,0,0,A).
generalizedFib(A,B,1,A,B).
generalizedFib(A,B,I,Prev,R) :- 
    I1 is I - 1,
    I1 > 0,
    generalizedFib(A,B,I1,Prev2,Prev),
    R is Prev2 + Prev.

fib(N,R) :- generalizedFib(0,1,N,_,R).


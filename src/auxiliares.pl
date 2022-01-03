
% retira n elementos de uma lista 

take(0, _, []) :- !.

take(_,[],[]).

take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).


%calcula custo de um caminho

calculaCusto([X,Y|[]],C1):-
    aresta(X,Y,C1).

calculaCusto([X,Y|XS],Custo):-
    aresta(X,Y,C1),
    calculaCusto([Y|XS],C2),
    Custo is C1 + C2.
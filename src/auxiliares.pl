:-include('aresta.pl').
:-include('veiculo.pl').


% retira n elementos de uma lista 

take(0, _, []) :- !.

take(_,[],[]).

take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).


%calcula custo de um caminho

calculaCusto([X,Y|[]],C1):-
    aresta(X,Y,C1,_).

calculaCusto([X,Y|XS],Custo):-
    aresta(X,Y,C1,_),
    calculaCusto([Y|XS],C2),
    Custo is C1 + C2.


calculaTempo([X,Y|[]],T1):-
	aresta(X,Y,_,T1).

calculaTempo([X,Y|XS],Tempo):-
    aresta(X,Y,_, T1),
    calculaCusto([Y|XS],T2),
    Tempo is T1 + T2.


getVertente(TipoVeiculo, Res) :-
	veiculo(TipoVeiculo, _, _, Res).


apagacabeca([],[]).
apagacabeca([X],[]).
apagacabeca([H|T],T).

remove(X,[X|R],R ).
remove(X,[Y|R],[Y|L]) :-
    X \= Y,
    remove(X, R, L).
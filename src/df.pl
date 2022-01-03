:-include('aresta.pl').


goal(gualtar/green-distribution).


dfs(Nodo, [Nodo|Caminho], C):-
	profundidadeprimeiro(Nodo, [Nodo], Caminho, C).

profundidadeprimeiro(Nodo, _, [], 0):-
	goal(Nodo).

profundidadeprimeiro(Nodo, Historico, [ProxNodo|Caminho], C):-
	adjacente(Nodo, ProxNodo, C1),
	nao(member(ProxNodo, Historico)),
	profundidadeprimeiro(ProxNodo, [ProxNodo|Historico], Caminho, C2),
    C is C1 + C2.

adjacente(Nodo, ProxNodo, C) :- aresta(Nodo, ProxNodo, C, _).

%------------------------------------Predicados Auxiliares-------------------------
nao(Questao) :- Questao, !, fail.
nao(_).

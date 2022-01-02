:-include('aresta.pl').

inicio(gualtar/green-distribution).

goal(maximinos/rua-do-cruzeiro).

% goal(lamacaes/rua-da-carreira).


% resolveDF(Res, Custo) :- 
%     inicio(EstadoInicial), resolveDF_Aux(EstadoInicial, [EstadoInicial], Sol, Custo).

% resolveDF_Aux(Nodo, HistoricoEstados, [Prox|Sol], Custo) :- 
%     aresta(Nodo, Prox, Custo1),
%     nao(member(Prox, HistoricoEstados)),
%     resolveDF_Aux(Prox, [Prox,HistoricoEstados], Sol, NovoCusto),
%     Custo is NovoCusto+1.






dfs(Nodo, [Nodo|Caminho], C):-
	profundidadeprimeiro(Nodo, [Nodo], Caminho, C), !.

profundidadeprimeiro(Nodo, _, [], 0):-
	goal(Nodo).

profundidadeprimeiro(Nodo, Historico, [ProxNodo|Caminho], C):-
	adjacente(Nodo, ProxNodo, C1),
	nao(member(ProxNodo, Historico)),
    writeln(ProxNodo),
    writeln(Historico),
	profundidadeprimeiro(ProxNodo, [ProxNodo|Historico], Caminho, C2),
    C is C1 + C2.

adjacente(Nodo, ProxNodo, C) :- aresta(Nodo, ProxNodo, C).
adjacente(Nodo, ProxNodo, C) :- aresta(ProxNodo, Nodo, C).

%------------------------------------Predicados Auxiliares-------------------------
nao(Questao) :- Questao, !, fail.
nao(_).

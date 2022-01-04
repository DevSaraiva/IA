% Includes
:-include('base-de-conhecimento.pl').

inicial(_).
final(gualtar/green-distribution).

goal(gualtar/green-distribution).


%--------------------------- estratégia de pesquisa informada gulosa tendo em conta CustoDistância ------------
adjacenteD([Nodo|Caminho]/CustoD/_, [ProxNodo,Nodo|Caminho]/NovoCusto/EstD):-
    aresta(Nodo, ProxNodo, PassoCustoD, _),
    \+ member(ProxNodo,Caminho),
    NovoCusto is CustoD + PassoCustoD,
    estima(ProxNodo,EstD,_).

resolve_gulosaD(Nodo,Caminho/CustoD) :- 
        estima(Nodo, EstimaD,_),
        agulosaD([[Nodo]/0/EstimaD], InvCaminho/CustoD/_),
        reverse(InvCaminho, Caminho),
        !.

agulosaD(Caminhos, Caminho) :-
    obtem_melhor_g_D(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

agulosaD(Caminhos, SolucaoCaminho) :-
    obtem_melhor_g_D(Caminhos,MelhorCaminho),
    remove(MehorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaD(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaD(NovoCaminhos,SolucaoCaminho).


obtem_melhor_g_D([Caminho],Caminho) :- !.
obtem_melhor_g_D([Caminho1/CustoD1/Est1, _/CustoD2/Est2|Caminhos], MelhorCaminho) :-
    Est1 =< Est2, !,
    obtem_melhor_g_D([Caminho1/CustoD1/Est1|Caminhos], MelhorCaminho).
obtem_melhor_g_D([_|Caminhos], MelhorCaminho) :- obtem_melhor_g_D(Caminhos, MelhorCaminho).

expande_gulosaD(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, adjacenteD(Caminho,NovoCaminho),ExpCaminhos).


%--------------------------- estratégia de pesquisa informada gulosa tendo em conta CustoTempo ------------

adjacenteT([Nodo|Caminho]/CustoD/_, [ProxNodo,Nodo|Caminho]/NovoCusto/EstT):-
    aresta(Nodo, ProxNodo, PassoCustoD, _),
    \+ member(ProxNodo,Caminho),
    NovoCusto is CustoD + PassoCustoD,
    estima(ProxNodo,_,EstT).

resolve_gulosaT(Nodo,Caminho/CustoD) :- 
        estima(Nodo,_, EstimaT),
        agulosaT([[Nodo]/0/EstimaT], InvCaminho/CustoD/_),
        reverse(InvCaminho, Caminho),
        !.

agulosaT(Caminhos, Caminho) :-
    obtem_melhor_g_T(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

agulosaT(Caminhos, SolucaoCaminho) :-
    obtem_melhor_g_T(Caminhos,MelhorCaminho),
    remove(MehorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaT(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaT(NovoCaminhos,SolucaoCaminho).


obtem_melhor_g_T([Caminho],Caminho) :- !.
obtem_melhor_g_T([Caminho1/CustoD1/Est1, _/CustoD2/Est2|Caminhos], MelhorCaminho) :-
    Est1 =< Est2, !,
    obtem_melhor_g_T([Caminho1/CustoD1/Est1|Caminhos], MelhorCaminho).
obtem_melhor_g_T([_|Caminhos], MelhorCaminho) :- obtem_melhor_g_T(Caminhos, MelhorCaminho).

expande_gulosaT(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, adjacenteT(Caminho,NovoCaminho),ExpCaminhos).


%--------------------------- estratégia de pesquisa informada estrela tendo em conta CustoDist ------------



adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est) :-
    aresta(Nodo,ProxNodo,PassoCusto, _),
    not(member(ProxNodo,Caminho)),
    NovoCusto is Custo + PassoCusto,
    estima(ProxNodo,Est,_).



obtem_melhor([Caminho],Caminho) :- !.
obtem_melhor([Caminho1/Custo1/Est1,_/Custo2/Est2|Caminhos],MelhorCaminho) :-
        Est1 + Custo1 =< Est2 + Custo2, !,
        obtem_melhor([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho).

obtem_melhor([_|Caminhos],MelhorCaminho) :-
        obtem_melhor(Caminhos,MelhorCaminho).


expande_aestrela(Caminho,ExpCaminhos) :-
        findall(NovoCaminho, adjacente2(Caminho,NovoCaminho),ExpCaminhos).


aestrela(Caminhos, Caminho) :-
        obtem_melhor(Caminhos, Caminho),
        Caminho = [Nodo|_]/_/_,
        goal(Nodo).

aestrela(Caminhos, SolucaoCaminho) :-
        obtem_melhor(Caminhos, MelhorCaminho),
        remove(MelhorCaminho, Caminhos, OutrosCaminhos),
        expande_aestrela(MelhorCaminho, ExpCaminhos),
        append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
        aestrela(NovoCaminhos, SolucaoCaminho).


resolve_aestrela(Nodo, Caminho/Custo) :-
        estima(Nodo, Estima,_),
        aestrela([[Nodo]/0/Estima],InvCaminho/Custo/_),
        reverse(InvCaminho,Caminho).


%--------------------------- estratégia de pesquisa não informada profundidade ------------

dfs(Nodo, [Nodo|Caminho], C):-
	profundidadeprimeiro(Nodo, [Nodo], Caminho, C).

profundidadeprimeiro(Nodo, _, [], 0):-
	goal(Nodo).

profundidadeprimeiro(Nodo, Historico, [ProxNodo|Caminho], C):-
	adjacente(Nodo, ProxNodo, C1),
	not(member(ProxNodo, Historico)),
	profundidadeprimeiro(ProxNodo, [ProxNodo|Historico], Caminho, C2),
    C is C1 + C2.

adjacente(Nodo, ProxNodo, C) :- aresta(Nodo, ProxNodo, C, _).

%--------------------------- estratégia de pesquisa não informada largura ------------
bfs(Nodo,Caminho,Custo):-
    goal(Final),
    bfs1(Final,[[Nodo]],Caminho),
    calculaCusto(Caminho,Custo).

bfs1(Final,[[Final|XS]|_],Caminho):- reverse([Final|XS],Caminho).

bfs1(Final,[Atuais|Outros],Caminho):-
    Atuais = [Act|_],
    Act \== Final,
    findall([X|Atuais], (aresta(Act,X,C1, _) ,not(member(X,Atuais))),Novos),
    append(Outros,Novos,Todos),
    bfs1(Final,Todos,Caminho).


%--------------------------- estratégia de pesquisa não informada - Busca Iterativa Limitada em Profundidade ------------

% resolve_limitada(Nodo,Solucao) Solucao é um caminho acíclico (na ordem % reversa) entre nó inicial No uma solução 
resolve_limitada(Nodo,Solucao/Custo) :-
    depthFirstIterativeDeepening(Nodo,Solucao),
    calculaCusto(Solucao,Custo).

% path(Nodo1,Nodo2,Caminho) encontra Caminho acíclico entre No1 e No2 
path(Nodo,Nodo,[Nodo]). % caminho com um único nó 
path(PrimeiroNodo,ProxNodo,[ProxNodo|Caminho]) :-
    path(PrimeiroNodo,Nodo,Caminho),                   % Há caminho até penúltimo 
    aresta(Nodo,ProxNodo,Custo, _),                      % Há nó anterior ao último
    not(member(ProxNodo,Caminho)).                    % evita um ciclo
 
% depthFirstIterativeDeepening(Nodo,Solução) iterativamente
% aumente a profundidade do caminho 
depthFirstIterativeDeepening(Nodo,Solucao) :-
    path(Nodo,Final,InvSolucao), 
    reverse(InvSolucao,Solucao),
    goal(Final).



% ----------------------------- funcoes auxiliares -----------------

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
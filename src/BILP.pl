% Style Check

:- style_check(-discontiguous).
:- style_check(-singleton).

% Includes
:- include('aresta.pl').


inicial(_).
final(gualtar/green-distribution).

goal(gualtar/green-distribution).

%--------------------------- estratégia de pesquisa não informada - Busca Iterativa Limitada em Profundidade ------------
calculaCusto([X,Y|[]],C1):-
    aresta(X,Y,C1).

calculaCusto([X,Y|XS],Custo):-
    aresta(X,Y,C1),
    calculaCusto([Y|XS],C2),
    Custo is C1 + C2.


% resolva(Nodo,Solucao) Solucao é um caminho acíclico (na ordem % reversa) entre nó inicial No uma solução 
resolva(Nodo,Solucao/Custo) :-
    depthFirstIterativeDeepening(Nodo,Solucao),
    calculaCusto(Solucao,Custo).

% path(Nodo1,Nodo2,Caminho) encontra Caminho acíclico entre No1 e No2 
path(Nodo,Nodo,[Nodo]). % caminho com um único nó 
path(PrimeiroNodo,ProxNodo,[ProxNodo|Caminho]) :-
    path(PrimeiroNodo,Nodo,Caminho),                   % Há caminho até penúltimo 
    aresta(Nodo,ProxNodo,Custo),                      % Há nó anterior ao último
    not(member(ProxNodo,Caminho)).                    % evita um ciclo
 
% depthFirstIterativeDeepening(Nodo,Solução) iterativamente
% aumente a profundidade do caminho 
depthFirstIterativeDeepening(Nodo,Solucao) :-
    path(Nodo,Final,InvSolucao), 
    reverse(InvSolucao,Solucao),
    goal(Final).

:- style_check(-discontiguous).
:- style_check(-singleton).


:-include('aresta.pl').


goal(gualtar/green-distribution).


bfs(Nodo,Caminho,Custo):-
    goal(Final),
    bfs1(Final,[[Nodo]],Caminho),
    calculaCusto(Caminho,Custo).

bfs1(Final,[[Final|XS]|_],Caminho):- reverse([Final|XS],Caminho).

bfs1(Final,[Atuais|Outros],Caminho):-
    Atuais = [Act|_],
    Act \== Final,
    findall([X|Atuais], (aresta(Act,X,C1) ,not(member(X,Atuais))),Novos),
    append(Outros,Novos,Todos),
    bfs1(Final,Todos,Caminho).



calculaCusto([X,Y|[]],C1):-
    aresta(X,Y,C1).

calculaCusto([X,Y|XS],Custo):-
    aresta(X,Y,C1),
    calculaCusto([Y|XS],C2),
    Custo is C1 + C2.
% Style Check

:- style_check(-discontiguous).
:- style_check(-singleton).

% Includes
:- include('aresta.pl').


inicial(_).
final(gualtar/green-distribution).

goal(gualtar/green-distribution).


%--------------------------- estratégia de pesquisa informada gulosa ------------
adjacente2([Nodo|Caminho]/Custo/_, [ProxNodo,Nodo|Caminho]/NovoCusto/Est):-
    aresta(Nodo, ProxNodo, PassoCusto, _),
    \+ member(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCusto,
    estima(ProxNodo,Est).

resolve_gulosa(Nodo,Caminho/Custo) :- 
        estima(Nodo, Estima),
        agulosa([[Nodo]/0/Estima], InvCaminho/Custo/_),
        reverse(InvCaminho, Caminho).

agulosa(Caminhos, Caminho) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).

agulosa(Caminhos, SolucaoCaminho) :-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    remove(MehorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosa(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosa(NovoCaminhos,SolucaoCaminho).


obtem_melhor_g([Caminho],Caminho) :- !.
obtem_melhor_g([Caminho1/Custo1/Est1, _/Custo2/Est2|Caminhos], MelhorCaminho) :-
    Est1 =< Est2, !,
    obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos], MelhorCaminho).
obtem_melhor_g([_|Caminhos], MelhorCaminho) :- obtem_melhor_g(Caminhos, MelhorCaminho).

expande_gulosa(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, adjacente2(Caminho,NovoCaminho),ExpCaminhos).



%------------------ Funções auxiliares -----------------------------------------

% Extensao do meta-predicado nao: Questao -> {V,F}
nao(Questao):-
    Questao, !, fail.
nao(Questao).

seleciona(E, [E|Xs],Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E,Xs,Ys).

inverso(Xs, Ys) :-
        inverso(Xss, [], Ys).
inverso([],Xs,Xs).
inverso([X|Xs],Ys,Zs) :- 
        inverso(Xs, [X|Ys], Zs).

remove(X,[X|R],R ).
remove(X,[Y|R],[Y|L]) :-
    X \= Y,
    remove(X, R, L).


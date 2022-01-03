
:- style_check(-discontiguous).
:- style_check(-singleton).


:-include('aresta.pl').

goal(gualtar/green-distribution).

remove(E,[E|Xs],Xs).
remove(E,[X|Xs],[X|Ys]) :- remove(E,Xs,Ys).

inverso(Xs, Ys):-
	inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
	inverso(Xs, [X|Ys], Zs).


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
        inverso(InvCaminho,Caminho).




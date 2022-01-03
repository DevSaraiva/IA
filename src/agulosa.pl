% Style Check

:- style_check(-discontiguous).
:- style_check(-singleton).

% Includes
:-include('aresta.pl').
:-include('auxiliares.pl').


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
        reverse(InvCaminho, Caminho).

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
        reverse(InvCaminho, Caminho).

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
:-include('entrega.pl').
:-include('estafeta.pl').
:-include('veiculo.pl').
:-include('auxiliares.pl').
:-include('circuito.pl').
:-include('agulosa.pl').
:-include('a-estrela.pl').
:-include('BILP.pl').
:-include('df.pl').
:-include('breath-first.pl').

:- op(900,xfy,'::').

:- dynamic encomenda/8.
:- dynamic circuito/2.


validar([]).
validar([A|T]):- A , validar(T).

insercao(Termo):- assert(Termo).
insercao(Termo):- retract(Termo), !, fail.

remover(Termo):- assert(Termo), !, fail.
remover(Termo):- retract(Termo).

evolucao( Termo ):- (
    findall(Invariante,+Termo::Invariante,Lista),
    insercao(Termo),
    validar(Lista)).



% Circuitos com mais entregas
% recebe x para mostrar top x caminhos
% recebe Solucao onde irá armazenar a lista


circuitosComMaisEntregas(N,Solucao) :-
    findall(Caminho, circuito(_,Caminho),Caminhos),
    circuitosComMaisEntregasAux(Caminhos,[],Solucao).
    





circuitosComMaisEntregasAux([],_,[]).

circuitosComMaisEntregasAux([Caminho|Caminhos],Visitados,Caminho/Counter|Res):-
        not(member(Caminho,Visitados)),
        calculaOcorrencias(Caminho,Caminhos,C),
        Counter is C + 1,
        circuitosComMaisEntregasAux(Caminhos,[Caminho|Visitados],Res).

circuitosComMaisEntregasAux([Caminho|Caminhos],Visitados,Res):-
        member(Caminho,Visitados),
        circuitosComMaisEntregasAux(Caminhos,Visitados,Res).




calculaOcorrencias(X,[],0).

calculaOcorrencias(X, [X|XS], Res) :-
    calculaOcorrencias(X,XS,Aux),
    Res is 1 + Aux.

calculaOcorrencias(X, [Y|XS], Res) :-
    calculaOcorrencias(X,XS,Aux),
    Res is Aux.





%decrescimos

decrescimo_bicicleta(VelocidadeMedia, Kgs, NovaVelocidadeMedia) :-
    Decrescimo is 0.7 * Kgs,
    NovaVelocidadeMedia is VelocidadeMedia - Decrescimo.

decrescimo_motos(VelocidadeMedia, Kgs, NovaVelocidadeMedia) :-
    Decrescimo is 0.5 * Kgs,
    NovaVelocidadeMedia is VelocidadeMedia - Decrescimo.

decrescimo_carro(VelocidadeMedia, Kgs, NovaVelocidadeMedia) :-
    Decrescimo is 0.1 * Kgs,
    NovaVelocidadeMedia is VelocidadeMedia - Decrescimo.



calcularEcologiaPorTempo(Distancia, Decrescimo, Tempo) :-    %o Decrescimo vem do predicado decrescimo_motos / bicicleta / carro
    Tempo is Distancia/Decrescimo.



indicadorDeProdutividade(Veiculo, Caminho, Tempo, Res) :-
    calculaCusto(Caminho, TotalDist),
    calculaTempo(Caminho, TotalCusto),
    getVertente(Veiculo, VertenteEco),
    Res is (TotalDist+Tempo) * VertenteEco.




escolheAlgoritmo(Alg, Nodo, Circuito/NovoCusto) :-
    Alg == 1 -> resolve_aestrela(Nodo, Caminho/Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);
    
    Alg == 2 -> resolve_gulosaD(Nodo, Caminho/Custo), % adicionei cut a esta pq da sempre a mesma solucao
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 3 -> dfs(Nodo, Caminho,Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 4 -> bfs(Nodo, Caminho,Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 5 -> resolve_limitada(Nodo, Caminho/Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    

    !,fail.

duplicaCaminho(IdaCaminhoAux/Custo,Caminho/NovoCusto) :-
    NovoCusto is Custo * 2,
    reverse(IdaCaminhoAux, VoltaCaminho),
    apagacabeca(IdaCaminhoAux,IdaCaminho),
    append(VoltaCaminho,IdaCaminho,Caminho).
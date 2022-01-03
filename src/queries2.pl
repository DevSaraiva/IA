:-include('entrega.pl').
:-include('estafeta.pl').
:-include('veiculo.pl').
:-include('auxiliares.pl').
:-include('caminho.pl').

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


findall(IdEstafeta, estafeta(IdEstafeta, _,Veiculo), Res).

circuitosComMaisEntregas(N,Solucao) :-
    findall(Caminho, caminho(_,Caminho),Caminhos),

    
    
    take(N,Res,Solucao).
    


circuitosComMaisEntregasAux([Caminho|Caminhos],Visitados,Caminho/Counter|Res):-
        not(member(Caminho,Visitados)),
        calculaOcorrencias(Caminho,Caminhos,C),
        Counter is C + 1,
        circuitosComMaisEntregasAux(Caminhos,[Caminho|Visitados],Res).






calculaOcorrencias(X,[],0).

calculaOcorrencias(X, [X|XS], Res) :-
    calculaOcorrencias(X,XS,Aux),
    Res is 1 + Aux.





%decrecimos

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



indicadorDeProdutividade(Distancia, Tempo, Res) :-
    Res is (Distancia+Tempo). 

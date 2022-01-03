:-include('entrega.pl').
:-include('estafeta.pl').
:-include('veiculo.pl').

:- op(900,xfy,'::').

:- dynamic encomenda/8.


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

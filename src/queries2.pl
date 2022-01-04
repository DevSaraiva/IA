:-include('algoritmos.pl').

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



circuitosComMaisEntregas(NumCircuitos, Solucao) :-
    findall(Caminho, circuito(_,Caminho),Caminhos),
    circuitosComMaisEntregasAux(Caminhos,[],Res),
    descending(Res,ResOrder),
    take(NumCircuitos,ResOrder,Solucao).
   
devolveCusto(Lista/Custo,Custo).


descending([], []).
descending([A], [A]).
descending(A,  [X,Y|C]) :-
  select(X, A, B),
  descending(B, [Y|C]),
        devolveCusto(X,W),
        devolveCusto(Y,Z),
          W   >=    Z.


circuitosComMaisEntregasAux([],_,[]) :- !.

circuitosComMaisEntregasAux([Caminho|Caminhos],Visitados,[Caminho/Counter|Res]):-
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




escolheAlgoritmo(Alg, Nodo, Circuito/NovoCusto) :-
    Alg == 1 -> resolve_aestrelaD(Nodo, Caminho/Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);
    
    Alg == 2 -> resolve_gulosaD(Nodo, Caminho/Custo), % adicionei cut a esta pq da sempre a mesma solucao
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 3 -> resolve_gulosaT(Nodo, Caminho/Custo), % adicionei cut a esta pq da sempre a mesma solucao
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 4 -> dfs(Nodo, Caminho,Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 5 -> bfs(Nodo, Caminho,Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 6 -> resolve_limitada(Nodo, Caminho/Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    

    !,fail.

duplicaCaminho(IdaCaminhoAux/Custo,Caminho/NovoCusto) :-
    NovoCusto is Custo * 2,
    reverse(IdaCaminhoAux, VoltaCaminho),
    apagacabeca(IdaCaminhoAux,IdaCaminho),
    append(VoltaCaminho,IdaCaminho,Caminho).






% CIRCUITOS COM MAIOR INDICADOR DE PRODUTIVIDADE

circuitosComMaiorProdutividade(NumCircuitos, Res) :-
    findall(circuito(Encomenda, Caminho), circuito(Encomenda,Caminho),Circuitos),
    circuitosComMaiorProdutividadeAux(Circuitos, [], Veiculo, Res),
    descending(Res,ResOrder),
    take(NumCircuitos,ResOrder,Solucao).



circuitosComMaiorProdutividadeAux([],_, _,[]) :- !.

circuitosComMaiorProdutividadeAux([Circuito|Circuitos],Visitados, Encomenda, [Caminho/Produtividade|Res]):-
        getCaminho(Circuito, Caminho),
        not(member(Caminho,Visitados)),
        getVeiculo(Encomenda, Veiculo),
        indicadorDeProdutividade(Circuito, Produtividade),
        circuitosComMaiorProdutividadeAux(Circuitos,[Caminho|Visitados], Encomenda, Res).

circuitosComMaiorProdutividadeAux([Circuito|Circuitos],Visitados, Encomenda, Res):-
        getCaminho(Circuito, Caminho),
        member(Caminho,Visitados),
        getVeiculo(Encomenda, Veiculo),
        circuitosComMaiorProdutividadeAux(Circuitos,Visitados, Encomenda, Res).




indicadorDeProdutividade(circuito(Encomenda, Caminho), Res) :-
    calculaCusto(Caminho, TotalDist),
    calculaTempo(Caminho, TotalTempo),
    getVeiculo(Encomenda, Veiculo),
    veiculo(Veiculo, _, _, VertenteEco),
    Res is (TotalDist+TotalTempo) * VertenteEco.


getCaminho(Circuito, ResCaminho) :- circuito(_, ResCaminho).

getVeiculo(encomenda(_, _, IdEstafeta, _, _, _, _), ResVeiculo) :-
    estafeta(IdEstafeta, _, ResVeiculo).

getEncomenda(Circuito, ResEncomenda) :-
    circuito(ResEncomenda, _).




decrescimo_bicicleta(VelocidadeMedia, Kgs, NovaVelocidadeMedia) :-
    Decrescimo is 0.7 * Kgs,
    NovaVelocidadeMedia is VelocidadeMedia - Decrescimo.

decrescimo_motos(VelocidadeMedia, Kgs, NovaVelocidadeMedia) :-
    Decrescimo is 0.5 * Kgs,
    NovaVelocidadeMedia is VelocidadeMedia - Decrescimo.

decrescimo_carro(VelocidadeMedia, Kgs, NovaVelocidadeMedia) :-
    Decrescimo is 0.7 * Kgs,
    NovaVelocidadeMedia is VelocidadeMedia - Decrescimo.



calcularTempo(Distancia, Veiculo, Peso, Tempo) :-    %o Decrescimo vem do predicado decrescimo_motos / bicicleta / carro
    Veiculo == carro -> decrescimo_carro(25, Peso, NovaVelocidadeMedia),
                        Tempo is Distancia/NovaVelocidadeMedia;
    Veiculo == mota -> decrescimo_motos(35, Peso, NovaVelocidadeMedia),
                        Tempo is Distancia/NovaVelocidadeMedia;
    Veiculo == bicicleta -> decrescimo_bicicleta(10, Peso, NovaVelocidadeMedia),
                        Tempo is Distancia/NovaVelocidadeMedia;
    !, fail.

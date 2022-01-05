:-include('algoritmos.pl').

:- op(900,xfy,'::').

:- dynamic encomenda/7.
:- dynamic circuito/2.
:- dynamic estafeta/3.
:- dynamic entrega/8.

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



contaEncomendas(Res) :- findall(IdEncomenda, encomenda(_, IdEncomenda, _, _, _, _, _), Sol), length(Sol, Res).
contaCircuitos(Res) :- findall(IdCircuito, circuito(IdCircuito, _), Sol), length(Sol, Res).
contaEstafetas(Res) :- findall(IdEstafeta, estafeta(IdEstafeta, _, _), Sol), length(Sol, Res).
contaEntregas(Res) :- findall(IdEntrega, entrega(IdEntrega, _, _, _, _, _, _, _), Sol), length(Sol, Res).

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

circuitosComMaiorProdutividade(NumCircuitos, Solucao) :-
    findall(circuito(IdEntrega, Caminho), circuito(IdEntrega,Caminho),Circuitos),
    circuitosComMaiorProdutividadeAux(Circuitos, [], IdEntrega, Res),
    descending(Res,ResOrder),
    take(NumCircuitos,ResOrder,Solucao).



circuitosComMaiorProdutividadeAux([],_, _,[]) :- !.

circuitosComMaiorProdutividadeAux([Circuito|Circuitos],Visitados, IdEntrega, [Caminho/Produtividade|Res]):-
        getCaminho(Circuito, Caminho),
        not(member(Caminho,Visitados)),
        getVeiculo(IdEntrega, Veiculo),
        indicadorDeProdutividade(Circuito, Produtividade),
        circuitosComMaiorProdutividadeAux(Circuitos,[Caminho|Visitados], IdEntrega, Res).

circuitosComMaiorProdutividadeAux([Circuito|Circuitos],Visitados, IdEntrega, Res):-
        getCaminho(Circuito, Caminho),
        member(Caminho,Visitados),
        getVeiculo(IdEntrega, Veiculo),
        circuitosComMaiorProdutividadeAux(Circuitos,Visitados, IdEntrega, Res).




indicadorDeProdutividade(circuito(IdEntrega, Caminho), Res) :-    % alterar para identrega
    calculaCusto(Caminho, TotalDist),
    calculaTempo(Caminho, TotalTempo),
    getVeiculo(IdEntrega, Veiculo),
    veiculo(Veiculo, _, _, VertenteEco),
    Res is (TotalDist+TotalTempo) * VertenteEco.


getCaminho(Circuito, ResCaminho) :- circuito(_, ResCaminho).

getVeiculo(IdEntrega, ResVeiculo) :-
    entrega(IdEntrega, IdEstafeta, _, _, _, _, _, _),
    estafeta(IdEstafeta, _, ResVeiculo).





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


% Comparar datas

compare_data(data(YY,MM,DD), = ,data(YY,MM,DD)).

compare_data(data(Y,M,D), > ,data(YY,MM,DD)) :-
        Y > YY.
compare_data(data(Y,M,D), > ,data(Y,MM,DD)) :-
        M > MM.
compare_data(data(Y,M,D), > ,data(Y,M,DD)) :-
        D > DD.

compare_data(data(Y,M,D), < ,data(YY,MM,DD)) :-
        Y < YY.

compare_data(data(Y,M,D), < ,data(Y,MM,DD)) :-
        M < MM.

compare_data(data(Y,M,D), < ,data(Y,M,DD)) :-
        D < DD.

%Comparar Horas

compare_hora(hora(H,M), =, hora(H,M)).

compare_hora(hora(H,M), > , hora(Hh,Mm)) :-
    H > Hh.
compare_hora(hora(Hh,M), > , hora(Hh,Mm)) :-
   M > Mm.

compare_hora(hora(H,M), <, hora(Hh,Mm)) :-
    H < Hh.
compare_hora(hora(Hh,M), < , hora(Hh,Mm)) :-
   M < Mm.




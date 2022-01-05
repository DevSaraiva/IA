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



contaEncomendas(Res) :- findall(Encomenda, encomenda(_, _, _, _, _, _, _), Sol), length(Sol, Res).

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

    Alg == 2 -> resolve_aestrelaT(Nodo, Caminho/Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);
            
    Alg == 3 -> resolve_gulosaD(Nodo, Caminho/Custo), % adicionei cut a esta pq da sempre a mesma solucao
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 4 -> resolve_gulosaT(Nodo, Caminho/Custo), % adicionei cut a esta pq da sempre a mesma solucao
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 5 -> dfs(Nodo, Caminho,Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 6 -> bfs(Nodo, Caminho,Custo),
                duplicaCaminho(Caminho/Custo,Circuito/NovoCusto);

    Alg == 7 -> resolve_limitada(Nodo, Caminho/Custo),
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
    Decrescimo is 0.1 * Kgs,
    NovaVelocidadeMedia is VelocidadeMedia - Decrescimo.


% da tempo em minutos
calcularTempo(Distancia, Veiculo, Peso, Tempo) :-    %o Decrescimo vem do predicado decrescimo_motos / bicicleta / carro
    Veiculo == carro -> decrescimo_carro(25, Peso, NovaVelocidadeMedia),
                        Tempo is (Distancia/NovaVelocidadeMedia) * 60;
    Veiculo == mota -> decrescimo_motos(35, Peso, NovaVelocidadeMedia),
                        Tempo is (Distancia/NovaVelocidadeMedia) * 60;
    Veiculo == bicicleta -> decrescimo_bicicleta(10, Peso, NovaVelocidadeMedia),
                        Tempo is (Distancia/NovaVelocidadeMedia) * 60;
    !, fail.








% encomenda(Freguesia/Rua,idEncomenda,idCliente, DataPrazo,TimePrazo, peso/volume, preco).

escolherCircuitoMaisEcologico(DataInicio/HoraInicio,IdEncomenda,) :- 
        encomenda(Destino,IdEncomenda,IdCliente,DataPrazo,HoraPrazo, Peso/Volume, Preco),
        escolheAlgoritmo(1,Destino,Caminho/Distancia), % algoritmo aestrela tendo em conta distancia
        atribuiEstafetaEco(Destino,Distancia,Peso,DataInicio/HoraInicio,DataPrazo/HoraPrazo,IdEstafetaAtri/Veiculo), % temos idestafeta e veiculo
        calcularTempo(Distancia,Veiculo,Peso,Tempo),
        somaDataHora(DataInicio/HoraInicio,Tempo,DataEntrega/HoraEntrega),
        % FALTA VER A CLASSIFICACAO
        evolucao(entrega(IdEncomenda,IdEstafetaAtri,IdCliente,Destino,DataPrazo/DataEntrega,       ,Peso/Volume,Preco)),
        evolucao(circuito(IdEncomenda,Caminho)).
    % QUESTOES : temos que fazer uma funcao que devolve todas as encomendas que ainda nao tem entrega(ou seja
    % que ainda nao foram entregues) para mostrar antes disto pq o estafeta nao vai saber de cor que encomendas 
    % ainda estao por entregar
    


atribuiEstafetaEco(Freguesia/Rua,Distancia,Peso,DataInicio/HoraInicio,DataPrazo/HoraPrazo,IdEstafetaAtri/Veiculo) :- 
            veiculosPossiveisPeso(Peso,VeiculosPossiveisPeso),
            veiculosPossiveisPrazo(Peso,Distancia,DataInicio/HoraInicio,DataPrazo/HoraPrazo,VeiculosPossiveisPrazo),
            conjuncaoListas(VeiculosPossiveisPeso,VeiculosPossiveisPrazo,Veiculos),
            findall(IdEstafeta,estafeta(IdEstafeta,Freguesia,Veiculo),member(Veiculo,Veiculos),Estafetas),
            ordenaEstafeta(...),



veiculosPossiveisPeso(Peso,Veiculos) :-
    ((Peso>0,Peso=<5)     -> Veiculos=[bicicleta,mota,carro];
     (Peso>5,Peso=<20)    -> Veiculos=[mota,carro];
     (Peso>20,Peso=<100)  -> Veiculos=[carro];
     ).

veiculosPossiveisPrazo(Peso,Distancia,DataInicio/HoraInicio,DataPrazo/HoraPrazo,Veiculos) :-
    calcularTempo(Distancia,bicicleta,Peso,TempoBicicleta),
    somaDataHora(DataInicio/HoraInicio,TempoBicicleta,DataentregaB/HoraEntregaB),
    checkPrazo(DataEntregaB/HoraEntregaB,DataPrazo/HoraPrazo,AnsB),
    ((AnsB == 1) -> Veiculos == [bicicleta,mota,carro];
      (AnsB == 0) -> (calcularTempo(Distancia,mota,Peso,TempoMota),
                    somaDataHora(DataInicio/HoraInicio,TempoMota,DataentregaM/HoraEntregaM),
                    checkPrazo(DataEntregaM/HoraEntregaM,DataPrazo/HoraPrazo,AnsM),
                    ((AnsM == 1) -> Veiculos = [mota,carro];
                     (AnsM == 0) -> Veiculo = [carro]).
                    );
    ).

somaDataHora(DataInicio/HoraInicio,Tempo,DataEntrega/HoraEntrega) :-
    % fazer depois

checkPrazo(DataFim/HoraFim,DataPrazo/HoraPrazo,Ans) :- % se for certo Ans = 1 senao = 0
            (  (compare_data(DataFim,<,DataPrazo)) -> Ans = 1;
               (compare_data(DataFim,=,DataPrazo),compare_hora(HoraFim,<,HoraPrazo)) -> Ans = 1;
               !,Ans = 0. ).

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




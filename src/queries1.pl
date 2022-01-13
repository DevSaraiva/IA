:-include('base-de-conhecimento.pl').

:-style_check(-singleton).


% Queries auxiliares


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


%-------------------------------------------------------------------------------------------------------
% Query 1 - identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;
% para testar:  encontraMaisEcologico(X).



findEstafetasPorVeiculo(Veiculo, Res) :-
                        findall(IdEstafeta, estafeta(IdEstafeta, _,Veiculo), Res). % Coloca no res a todos os ids de estafeta que usaram determinado veiculo


calculaEntregasEstafeta(IdEstafeta,Res) :-
        findall(IdEntrega,entrega(IdEntrega, IdEstafeta, _, _, _, _, _, _, _),Lista),
        length(Lista,Res).



descending([], []).
descending([A], [A]).
descending(A,  [X,Y|C]) :-
  select(X, A, B),
  descending(B, [Y|C]),
        calculaEntregasEstafeta(X,W),
        calculaEntregasEstafeta(Y,Z),
          W   >=    Z.



iguais([X|[]],1).

iguais([X,Y|Xs], 1) :-  calculaEntregasEstafeta(X,Z),
                        calculaEntregasEstafeta(Y,W),
                        Z \= W.

iguais([X,Y|Xs],Res) :- calculaEntregasEstafeta(X,Z),
                        calculaEntregasEstafeta(Y,W),
                        Z = W,
                        iguais([Y|Xs],Res2),
                        Res is 1 + Res2.


maisEcologico(Lista,Res):-
        iguais(Lista,Iguais),
        take(Iguais,Lista,Res).



encontraMaisEcologico(Res) :- 
        findEstafetasPorVeiculo(bicicleta,ListaEstafetasBicicleta),
        length(ListaEstafetasBicicleta,L),
        L > 0,
        descending(ListaEstafetasBicicleta,Lista),
        maisEcologico(Lista,Res),
        !.


encontraMaisEcologico(Res) :- 
        findEstafetasPorVeiculo(mota,ListaEstafetasMota),
        length(ListaEstafetasMota,L),
        L > 0,
        descending(ListaEstafetasMota,Lista),
        maisEcologico(Lista,Res),
        !.

encontraMaisEcologico(Res) :- 
        findEstafetasPorVeiculo(carro,ListaEstafetasCarro),
        length(ListaEstafetasCarro,L),
        L > 0,
        descending(ListaEstafetasCarro,Lista),
        maisEcologico(Lista,Res),
        !.


%------------------------------------------------------------------------------------------


%QUERY 2 - identificar  que  estafetas  entregaram  determinada(s)  entrega(s)  a  um determinado cliente;
% para testar:  entregas_do_estafeta_PorCliente(bernardo, [portatil, forno], [], Res).

entregas_do_estafeta_PorCliente(_, [], Lista, Lista).

entregas_do_estafeta_PorCliente(IdCliente, [IdEntrega|Xs], Lista, Res) :- 
        findall(IdEstafeta, entrega(IdEntrega, IdEstafeta, IdCliente, _, _ , _, _, _, _), S),
        append(Lista, S, L),
        entregas_do_estafeta_PorCliente(IdCliente, Xs, L, Res),
        !.


%------------------------------------------------------------------------------------------

%QUERY 3 - identificar os clientes servidos por um determinado estafeta; 
%para testar:   clientesPorEstafeta(ze_joao, X).

clientesPorEstafeta(IdEstafeta, Res) :- 
        findall(IdCliente, entrega(_, IdEstafeta, IdCliente, _, _, _, _, _, _), Res).


%-----------------------------------------------------------------------------------------

%QUERY 4 - calcular o valor faturado pela Green Distribution num determinado dia; (Considerando no dia em que a entrega foi entregue)
%para testar: faturacaoDiaria(data(2021,1,29), Res).

somaElementos([], 0).
somaElementos([H|Xs], Res) :- somaElementos(Xs, Sum), Res is Sum+H.


faturacaoDiaria(DataEntrega, Res) :- 
        findall(Preco, entrega(_, _, _, _, _ , DataEntrega/_, _, _, Preco), S),
        somaElementos(S, Res).


%-----------------------------------------------------------------------------------------

%QUERY 5 - identificar  quais  as  zonas  (e.g.,  rua  ou  freguesia)  com  maior  volume  de entregas por parte da Green Distribution; 
%para testar:   zonasComMaisEntregas(Res).


listaDasZonas(Res) :- 
        findall(Zona, entrega(_, _, _, Zona, _, _, _, _, _), S), sort(S, Res).       %deve haver uma soluçao melhor que nao devolva repetidos sem ter que fazer o sort... TESTAR COM setof


%para testar:   entregasPorZona(roriz/pidre, Res).
entregasPorZona(Zona, Res) :- 
        findall(Zona, entrega(_, _, _, Zona, _, _, _, _, _), S),
        length(S, Res).

%para testar:   entregasPorZonaLista([roriz/pidre], [], Res).
entregasPorZonaLista([], Lista, Lista).
entregasPorZonaLista([Zona|Xs], Lista, Res) :- 
        entregasPorZona(Zona, X1),                      %tentar descobrir o Max e fazer a lista só com os que tiverem o Max (assim nao precisava de ordenar)
        append(Lista, [Zona/X1], L3),      
        entregasPorZonaLista(Xs, L3, Res).

zonasComMaisEntregas(Res) :- listaDasZonas(ListaZonas), entregasPorZonaLista(ListaZonas, [], Lista), pair_sort(Lista, Res).

%Auxiliar que ordena a lista de tuplos
:-use_module(library(clpfd)).

swap_internals((X/Y), Y1-X):- Y1 #= -Y.

pair_sort(L,Sorted):- 
      maplist(swap_internals, L, L2),
      keysort(L2, L3),
      maplist(swap_internals, Sorted, L3).


%-----------------------------------------------------------------------------------------

%QUERY 6 - calcular a classificação média de satisfação de cliente para um determinado estafeta;
%para testar: classificacaoDoClienteParaEstafeta(ze_joao,Res).


somaLista([],0).
somaLista([H|T],S) :-
        somaLista(T,G),
        S is H+G.

classificacaoDoClienteParaEstafeta(IdEstafeta,Res) :- 
        findall(Classificacao,entrega(_, IdEstafeta, _, _, _, _, Classificacao, _, _), Lista),
        somaLista(Lista,S),
        length(Lista,T),
        Res is S / T.

%-----------------------------------------------------------------------------------------

% QUERY 7 - identificar o número total de entregas pelos diferentes meios de transporte,num determinado intervalo de tempo;
% para testar: numeroTotalEntregas(data(2020,1,1)/data(2022,12,30),EntregasBicicleta,EntregasCarro,EntregasMoto).

listaEntregasDurante(DataI/DataF,CL) :-
        findall(Data/IdEstafeta,entrega(_ , IdEstafeta, _, _, _, Data/_, _, _, _), L),
        removeListaEntregasForaDoIntervalo(DataI/DataF, L, CL).


removeListaEntregasForaDoIntervalo(_, [], []).

removeListaEntregasForaDoIntervalo(DataI/DataF, [X/IdEstafeta|XS], Res):-
        compare_data(X, >, DataI), 
        compare_data(X, <, DataF),
        removeListaEntregasForaDoIntervalo(DataI/DataF, XS, Y),
        append([X/IdEstafeta], Y, Res).

 removeListaEntregasForaDoIntervalo(DataI/DataF, [X|XS], Res) :-
        removeListaEntregasForaDoIntervalo(DataI/DataF, XS, Res).

criaListaVeiculo([],Lista,Lista).
criaListaVeiculo([_/Id|Estafetas],Lista, Res) :-
        findall(Veiculo,estafeta(Id,_,Veiculo),V),
        append(Lista,V,L8),
        criaListaVeiculo(Estafetas,L8,Res).


contaBicicletas([],0).
contaBicicletas([bicicleta|T],N) :- 
        contaBicicletas(T,N1), N is N1 + 1.
contaBicicletas([X|T],N) :- 
        X \= bicicleta,
        contaBicicletas(T,N).

contaCarros([],0).
contaCarros([carro|T],N) :- 
        contaCarros(T,N1), N is N1 + 1.
contaCarros([X|T],N) :- 
        X \= carro,
        contaCarros(T,N).

contaMotas([],0).
contaMotas([mota|T],N) :- 
        contaMotas(T,N1), N is N1 + 1.
contaMotas([X|T],N) :- 
        X \= mota,
        contaMotas(T,N).
        


numeroTotalEntregas(DataI/DataF,EntregasBicicleta,EntregasCarro,EntregasMoto) :-
        listaEntregasDurante(DataI/DataF,ListaEstafetas),
        criaListaVeiculo(ListaEstafetas,[],ListaVeiculos),
        contaBicicletas(ListaVeiculos,EntregasBicicleta),
        contaCarros(ListaVeiculos,EntregasCarro),
        contaMotas(ListaVeiculos,EntregasMoto),
        !.

%---------------------------------------------------------------------------------------

% Query 8 - identificar o número total de entregas pelos estafetas, num determinado
%intervalo de tempo;
%para testar:   entregasDurante(data(2021,5,19)/data(2022,12,31), Res).

entregasDurante(DataI/DataF,Res) :-
        findall(Data,entrega(_, _, _, _, _, Data/_, _, _, _), L),
        removeEntregasForaDoIntervalo(DataI/DataF, L, CL),
        length(CL,Res),
        !.

removeEntregasForaDoIntervalo(_, [], []).

removeEntregasForaDoIntervalo(DataI/DataF, [X|XS], Res):-
        compare_data(X, >, DataI), 
        compare_data(X, <, DataF),
        removeEntregasForaDoIntervalo(DataI/DataF, XS, Y),
        append([X], Y, Res).

removeEntregasForaDoIntervalo(DataI/DataF, [X|XS], Res) :-
        removeEntregasForaDoIntervalo(DataI/DataF, XS, Res).
        

        

%---------------------------------------------------------------------------------------

%QUERY 9: calcular  o  número  de  entregas  entregues  e  não  entregues  pela  Green Distribution, num determinado período de tempo;
%para testar:   calculaNEntregasIntervalo(data(2021,1,1)/data(2021,12,19), Entregues, NaoEntregues).
calculaNEntregasIntervalo(DataI/DataF, ResEntregues, ResNaoEntregues) :- 
        findall(IdEntrega,entrega(IdEntrega, _, _, _, _, _, _, _, _), L),
        length(L, TotalEntregas),
        entregasDurante(DataI/DataF, ResEntregues),
        ResNaoEntregues is (TotalEntregas - ResEntregues),
        !.



%---------------------------------------------------------------------------------------

%QUERY 10 - calcular o peso total transportado por estafeta num determinado dia.
%para testar: pesoTotalPorEstafetas(data(2021, 1, 29), Lista).


% para testar: pesoTotalPorEstafeta(ze_joao,PT).
pesoTotalPorEstafeta(IdEstafeta, Data, PT) :-
        findall(Peso, entrega(_, IdEstafeta, _, _, _, Data/_, _, Peso/_ , _), S),
        somaElementos(S, PT).


% para testar: pesoTotalPorEstafetasLista([ze_joao,rui],[],Lista).
pesoTotalPorEstafetasLista([], Data, Lista, Lista).
pesoTotalPorEstafetasLista([Estafeta|Estafetas], Data, Lista, Res) :-
        pesoTotalPorEstafeta(Estafeta, Data, PT),
        append(Lista, [Estafeta/PT], L10),
        pesoTotalPorEstafetasLista(Estafetas, Data, L10, Res). 
         

pesoTotalPorEstafetas(Data, Res) :-
        findall(IdEstafeta, estafeta(IdEstafeta, _, _),ListaEstafetas),
        pesoTotalPorEstafetasLista(ListaEstafetas, Data, [], Res), writeln(Res).

        

%------------------------------------------------------------------------------------------

% retira n elementos de uma lista 

take(0, _, []) :- !.

take(_,[],[]).

take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).

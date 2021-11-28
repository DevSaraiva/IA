:-style_check(-singleton).

% veiculo (tipo de veiculo, velocidade_media, cargaMax, vertente_ecologica)

veiculo(bicicleta, 10, 5, 50).
veiculo(mota, 35, 20, 30).
veiculo(carro, 25, 100, 10). 


% estafeta(idEstafeta, freguesia, veiculo)

estafeta(joaquim, vila-caiz, carro).
estafeta(painatal, roriz, carro).
estafeta(rui, roriz, mota).
estafeta(ze-joao, barcelos, bicicleta).
estafeta(miguel, braga, bicicleta).
estafeta(margarida, guimaraes, mota).




%entrega(idEncomenda, idEstafeta, idCliente, freguesia/rua , dataMax/dataEntrega , classificação, peso/volume, preço)

entrega(televisao, joaquim, manuel, vila-caiz/aldeia-nova, date(2021, 1, 30)/date(2021, 1, 29),5, 30/80, 10).
entrega(portatil, rui, bernardo, roriz/pidre, date(2021, 2, 12)/date(2021, 2, 11),4 , 12/30, 5).
entrega(telemovel, ze-joao, miguel, barcelos/pedreira, date(2021, 3, 10)/date(2021, 3, 9), 3, 3/10, 3).

entrega(forno, painatal, bernardo, roriz/pidre, date(2021, 2, 12)/date(2021, 3, 29), 1, 12/30, 5).
entrega(telemovel, margarida, pedro, vila-caiz/aldeia-nova, date(2021, 3, 10)/date(2021, 3, 9), 2, 3/10, 3).

entrega(teclado, rui, alberto, esposende/margem, date(2021, 6, 19)/date(2021, 5, 22), 4, 21/30, 4).
entrega(rato, miguel, joao, esposende/margem, date(2021, 6, 22)/date(2021, 6, 22), 5, 2/30, 50).
entrega(headset, margarida, ana, esposende/margem, date(2021, 12, 30)/date(2021, 12, 29), 3, 9/30, 24).




% Queries auxiliares

% Gives the length of a list.
listlength([]     , 0 ).
listlength([_|Xs] , L ):- 
    listlength(Xs,N), 
    L is N+1. 


% Comparar datas

compare_date(date(YY,MM,DD), = ,date(YY,MM,DD)).

compare_date(date(Y,M,D), > ,date(YY,MM,DD)) :-
        Y > YY.
compare_date(date(Y,M,D), > ,date(Y,MM,DD)) :-
        M > MM.
compare_date(date(Y,M,D), > ,date(Y,M,DD)) :-
        D > DD.

compare_date(date(Y,M,D), < ,date(YY,MM,DD)) :-
        Y < YY.

compare_date(date(Y,M,D), < ,date(Y,MM,DD)) :-
        M < MM.

compare_date(date(Y,M,D), < ,date(Y,M,DD)) :-
        D < DD.


%-------------------------------------------------------------------------------------------------------
% Query 1 - identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;
% para testar:  encontraMaisEcologico(_, X).

findEstafetasPorVeiculo(Veiculo, Res) :-
                        findall(IdEstafeta/TotalEntregas, estafeta(IdEstafeta, _,Veiculo), Res). % Coloca no res a todos os ids de estafeta que usaram determinado veiculo


maisEcologico([IdEstafeta/_|[]],IdEstafeta).

maisEcologico([IdEstafeta1/TotalEntregas1, _/TotalEntregas2  |Xs],Ecologico) :-
                        TotalEntregas1 > TotalEntregas2,
                        maisEcologico([IdEstafeta1/TotalEntregas1|Xs],Ecologico).

maisEcologico([_/TotalEntregas1, IdEstafeta2/TotalEntregas2  |Xs],Ecologico) :-
                        TotalEntregas1 =< TotalEntregas2,
                        maisEcologico([IdEstafeta2/TotalEntregas2|Xs],Ecologico).
                         

encontraMaisEcologico(ListaEstafetasBicicleta,Res) :- 
        findEstafetasPorVeiculo(bicicleta,ListaEstafetasBicicleta),
        listlength(ListaEstafetasBicicleta,L),
        L > 0,
        maisEcologico(ListaEstafetasBicicleta,Res).


encontraMaisEcologico(ListaEstafetasMota,Res) :- 
        findEstafetasPorVeiculo(mota,ListaEstafetasMota),
        listlength(ListaEstafetasMota,L),
        L > 0,
        maisEcologico(ListaEstafetasMota,Res).

encontraMaisEcologico(ListaEstafetasCarro,Res) :- 
        findEstafetasPorVeiculo(carro,ListaEstafetasCarro),
        listlength(ListaEstafetasCarro,L),
        L > 0,
        maisEcologico(ListaEstafetasCarro,Res).


%------------------------------------------------------------------------------------------


%QUERY 2 - identificar  que  estafetas  entregaram  determinada(s)  encomenda(s)  a  um determinado cliente;
% para testar:  encomendas_do_estafeta_PorCliente(bernardo, [portatil, forno], [], Res).

encomendas_do_estafeta_PorCliente(_, [], Lista, Lista).

encomendas_do_estafeta_PorCliente(Cliente, [IdEncomenda|Xs], Lista, Res) :- 
        findall(IdEstafeta, entrega(IdEncomenda, IdEstafeta, IdCliente, _, _ , _, _, _), S),
        append(Lista, S, L),
        encomendas_do_estafeta_PorCliente(Cliente, Xs, L, Res).
%------------------------------------------------------------------------------------------

%QUERY 3 - identificar os clientes servidos por um determinado estafeta; 
%para testar:   clientesPorEstafeta(ze-joao, X).

clientesPorEstafeta(IdEstafeta, Res) :- 
        findall(IdCliente, entrega(_, IdEstafeta, IdCliente, _, _, _, _, _), Res).


%-----------------------------------------------------------------------------------------

%QUERY 4 - calcular o valor faturado pela Green Distribution num determinado dia;
%para testar: faturacaoDiaria(date(2021,1,29), Res).

somaElementos([], 0).
somaElementos([H|Xs], Res) :- somaElementos(Xs, Sum), Res is Sum+H.


faturacaoDiaria(DataEntrega, Res) :- 
        findall(Preco, entrega(_, _, _, _, _/DataEntrega , _, _, Preco), S),
        somaElementos(S, Res).


%-----------------------------------------------------------------------------------------

%QUERY 5 - identificar  quais  as  zonas  (e.g.,  rua  ou  freguesia)  com  maior  volume  de entregas por parte da Green Distribution; 
%para testar:   zonasComMaisEntregas(Res).


listaDasZonas(Res) :- 
        findall(Zona, entrega(_, _, _, Zona, _, _, _, _), S), sort(S, Res).       %deve haver uma soluçao melhor que nao devolva repetidos sem ter que fazer o sort... TESTAR COM setof


%para testar:   entregasPorZona(roriz/pidre, Res).
entregasPorZona(Zona, Res) :- 
        findall(Zona, entrega(_, _, _, Zona, _, _, _, _), S),
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
%para testar: classificacaoDoClienteParaEstafeta(ze-joao,Res).


somaLista([],0).
somaLista([H|T],S) :-
        somaLista(T,G),
        S is H+G.

classificacaoDoClienteParaEstafeta(IdEstafeta,Res) :- 
        findall(Classificacao,entrega(_, IdEstafeta, _, _, _, Classificacao, _, _), Lista),
        somaLista(Lista,S),
        length(Lista,T),
        Res is S / T.

%-----------------------------------------------------------------------------------------

% QUERY 7 - identificar o número total de entregas pelos diferentes meios de transporte,num determinado intervalo de tempo;
% para testar: numeroTotalEntregas(date(2000,1,1),date(2010,12,30),EntregasBicicleta,EntregasCarro,EntregasMoto).

listaEntregasDurante(DataI/DataF,CL) :-
        findall(Data/IdEstafeta,entrega(_ , IdEstafeta, _, _, _/Data, _, _, _), L),
        removeListaEntregasForaDoIntervalo(DataI/DataF, L, CL).


removeListaEntregasForaDoIntervalo(_, [], []) :- !.

removeListaEntregasForaDoIntervalo(DataI/DataF, [X/IdEstafeta|XS], Res):-
        !,
        compare_date(X, >, DataI), 
        compare_date(X, <, DataF),
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
        


numeroTotalEntregas(DataI,DataF,EntregasBicicleta,EntregasCarro,EntregasMoto) :-
        listaEntregasDurante(DataI/DataF,ListaEstafetas),
        criaListaVeiculo(ListaEstafetas,[],ListaVeiculos),
        contaBicicletas(ListaVeiculos,EntregasBicicleta),
        contaCarros(ListaVeiculos,EntregasCarro),
        contaMotas(ListaVeiculos,EntregasMoto),
        !.

%---------------------------------------------------------------------------------------

% Query 8 - identificar o número total de entregas pelos estafetas, num determinado
%intervalo de tempo;

entregasDurante(DataI/DataF,Res) :-
        findall(Data,entrega(_, _, _, _, _/Data, _, _, _), L),
        removeEntregasForaDoIntervalo(DataI/DataF, L, CL),
        length(CL,Res).

removeEntregasForaDoIntervalo(_, [], []) :- !.

removeEntregasForaDoIntervalo(DataI/DataF, [X|XS], Res):-
        !,
        compare_date(X, >, DataI), 
        compare_date(X, <, DataF),
        removeEntregasForaDoIntervalo(DataI/DataF, XS, Y),
        append([X], Y, Res).

removeEntregasForaDoIntervalo(DataI/DataF, [X|XS], Res) :-
        !,
        removeEntregasForaDoIntervalo(DataI/DataF, XS, Res).
        

        

%---------------------------------------------------------------------------------------

%QUERY 9: calcular  o  número  de  encomendas  entregues  e  não  entregues  pela  Green Distribution, num determinado período de tempo;
%para testar:   calculaNEncomendasIntervalo(date(2000,1,1)/date(2021,1,1), Entregues, NaoEntregues).
calculaNEncomendasIntervalo(DataI/DataF, ResEntregues, ResNaoEntregues) :- 
        findall(IdEncomenda,entrega(IdEncomenda, _, _, _, _, _, _, _), L),
        length(L, TotalEncomendas),
        entregasDurante(DataI/DataF, ResEntregues),
        ResNaoEntregues is (TotalEncomendas - ResEntregues).



%---------------------------------------------------------------------------------------

%QUERY 10 - calcular o peso total transportado por estafeta num determinado dia.
%para testar: pesoTotalPorEstafetas(Lista)


% para testar: pesoTotalPorEstafeta(ze-joao,PT).
pesoTotalPorEstafeta(IdEstafeta,PT) :-
        findall(Peso, entrega(_, IdEstafeta, _, _, _/DataEntrega , _, Peso/_ , _), S),
        somaElementos(S, PT).


% para testar: pesoTotalPorEstafetasLista([ze-joao,rui],[],Lista).
pesoTotalPorEstafetasLista([], Lista, Lista).
pesoTotalPorEstafetasLista([Estafeta|Estafetas],Lista,Res) :-
        pesoTotalPorEstafeta(Estafeta,PT),
        append(Lista, [Estafeta/PT], L10),
        pesoTotalPorEstafetasLista(Estafetas,L10,Res). 
         

pesoTotalPorEstafetas(Res) :-
        findall(IdEstafeta, estafeta(IdEstafeta, _, _),ListaEstafetas),
        pesoTotalPorEstafetasLista(ListaEstafetas,[],Res).

        


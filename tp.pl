:-style_check(-singleton).

% veiculo (tipo de veiculo, velocidade_media, cargaMax, vertente_ecologica)

veiculo(bicicleta, 10, 5, 50).
veiculo(mota, 35, 20, 30).
veiculo(carro, 25, 100, 10). 


% estafeta(idEstafeta, totalEntregas, freguesia, veiculo, avaliacao)

estafeta(joaquim, 10, vila-caiz, carro, 5).
estafeta(rui, 20, roriz, mota, 4).
estafeta(ze-joao, 30, barcelos, bicicleta,3).



%entrega(idEncomenda, idEstafeta, idCliente, freguesia/rua , dataMax/dataEntrega , classificação, peso/volume, preço)

entrega(televisao, joaquim, manuel, vila-caiz/aldeia-nova, date(2007, 1, 30)/date(2007, 1, 29),5, 30/80, 10).
entrega(portatil, rui, bernardo, roriz/pidre, date(2008, 2, 12)/date(2008, 2, 11),4 , 12/30, 5).
entrega(telemovel, ze-joao, miguel, barcelos/pedreira, date(2008, 3, 10)/date(2008, 3, 9), 3, 3/10, 3).

entrega(forno, painatal, bernardo, roriz/pidre, date(2008, 2, 12)/date(2007, 1, 29),4 , 12/30, 5).
entrega(telemovel, ze-joao, pedro, vila-caiz/aldeia-nova, date(2008, 3, 10)/date(2008, 3, 9), 3, 3/10, 3).

entrega(teclado, rui, alberto, esposende/margem, date(2008, 2, 12)/date(2007, 1, 29),4 , 12/30, 5).
entrega(teclado, miguel, joao, esposende/margem, date(2008, 2, 12)/date(2007, 1, 29),4 , 12/30, 5).
entrega(teclado, margarida, ana, esposende/margem, date(2008, 2, 12)/date(2007, 1, 29),4 , 12/30, 5).




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
                        findall(IdEstafeta/TotalEntregas, estafeta(IdEstafeta, TotalEntregas,_,Veiculo,_), Res). % Coloca no res a todos os ids de estafeta que usaram determinado veiculo


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
%para testar: faturacaoDiaria(date(2007,1,29), Res).

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
        

        
      


%QUERY 9: calcular  o  número  de  encomendas  entregues  e  não  entregues  pela  Green Distribution, num determinado período de tempo;
%para testar:   calculaNEncomendasIntervalo(date(2000,1,1)/date(2008,1,1), Entregues, NaoEntregues).
calculaNEncomendasIntervalo(DataI/DataF, ResEntregues, ResNaoEntregues) :- 
        findall(IdEncomenda,entrega(IdEncomenda, _, _, _, _, _, _, _), L),
        length(L, TotalEncomendas),
        entregasDurante(DataI/DataF, ResEntregues),
        ResNaoEntregues is (TotalEncomendas - ResEntregues).
        


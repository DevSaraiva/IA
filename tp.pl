:- use_module(library(date)).
:-style_check(-singleton).

% veiculo (tipo de veiculo, velocidade_media, cargaMax, vertente_ecologica)

veiculo(bicicleta, 10, 5, 50).
veiculo(mota, 35, 20, 30).
veiculo(carro, 25, 100, 10). 


% estafeta(idEstafeta, totalEntregas, freguesia, veiculo, avaliacao)

estafeta(joaquim, 10, vila-caiz, carro, 5).
estafeta(rui, 20, roriz, mota, 4).
estafeta(ze-joao, 30, barcelos, bicicleta,3).



%entrega(idEstafeta, freguesia/rua , dataMax/dataEntrega , classificação, peso/volume, preço)

entrega(joquim,vila-caiz/aldeia-nova, date(2007, 1, 30)/date(2007, 1, 29),5, 30/80, 10).
entrega(rui,roriz/pidre, date(2008, 2, 12)/date(2008, 2, 11),4 , 12/30, 5).
entrega(ze-joao, barcelos/pedreira, date(2008, 3, 10)/date(2008, 3, 9), 3, 3/10, 3).


% Gives the length of a list.
listlength([]     , 0 ).
listlength([_|Xs] , L ):- 
    listlength(Xs,N), 
    L is N+1. 



% Query 1 - identificar o estafeta que utilizou mais vezes um meio de transporte mais ecológico;


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




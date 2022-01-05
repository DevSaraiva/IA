:- op( 900,xfy,'::' ).



%grafo

+aresta(Inicio, Fim, _, _) :: (findall((Inicio,Fim),(aresta(Inicio,Fim, _, _)),Sol), length(Sol,N), N < 1).

aresta(palmeira/rua-do-rio,palmeira/rua-da-poca,3.4,6).
aresta(palmeira/rua-do-rio,palmeira/rua-do-monte,3.1,5).
aresta(palmeira/rua-do-monte,palmeira/rua-da-poca,2.4,4).
aresta(palmeira/rua-do-rio,real/rua-dos-paiois,8.5,12).
aresta(palmeira/rua-da-poca,maximinos/rua-do-caires,6.5,9).
aresta(palmeira/rua-do-monte,maximinos/rua-da-naia,7,10).
aresta(maximinos/rua-da-naia,maximinos/rua-de-caires,2.2,5).
aresta(maximinos/rua-da-naia,maximinos/rua-do-cruzeiro,0.65,2).
aresta(maximinos/rua-do-cruzeiro,maximinos/rua-de-caires,2,4).
aresta(maximinos/rua-do-cruzeiro,real/rua-do-tojal,1.8,4).
aresta(real/rua-do-tojal,real/rua-dos-paiois,0.4,2).
aresta(real/rua-do-tojal,real/rua-das-giestas,0.3,2).
aresta(real/rua-dos-paiois,real/rua-das-giestas,0.35,1).
aresta(sao-vitor/rua-do-taxa,real/rua-das-giestas,6.3,9).
aresta(sao-vitor/rua-do-taxa,sao-vitor/rua-da-fabrica,1.2,4).
aresta(sao-vitor/rua-da-fabrica,sao-vitor/rua-dom-pedro-v,0.8,2).
aresta(sao-vitor/rua-do-taxa,sao-vitor/rua-dom-pedro-v,1.6,3).
aresta(sao-vitor/rua-da-fabrica,nogueiro/rua-da-capela,3.1,6).
aresta(sao-vitor/rua-dom-pedro-v,gualtar/rua-do-fontelo,2.7,5).
aresta(sao-vitor/rua-do-taxa,lamacaes/rua-da-torre,3.9,8).
aresta(gualtar/rua-do-fontelo,gualtar/rua-do-fontao,2.3,5).
aresta(gualtar/rua-do-fontao,gualtar/green-distribution,2.9,6).
aresta(gualtar/green-distribution,gualtar/rua-breias,2.4,5).
aresta(gualtar/rua-breias,nogueiro/rua-do-major,5,9).
aresta(nogueiro/rua-do-major,nogueiro/rua-da-rasa,0.4,2).
aresta(nogueiro/rua-da-rasa,nogueiro/rua-da-capela,0.75,3).
aresta(nogueiro/rua-da-capela,nogueiro/rua-do-major,0.65,2).
aresta(gualtar/rua-do-fontao,lamacaes/rua-da-carreira,4.9,9).
aresta(lamacaes/rua-da-carreira,lamacaes/rua-da-torre,0.7,2).
aresta(lamacaes/rua-da-torre,lamacaes/rua-do-passal,0.17,1).
aresta(lamacaes/rua-do-passal,nogueiro/rua-da-rasa,1.6,4).

%sentido inverso 
aresta(palmeira/rua-da-poca,palmeira/rua-do-rio,3.4,6).
aresta(palmeira/rua-do-monte,palmeira/rua-do-rio,3.1,5).
aresta(palmeira/rua-da-poca,palmeira/rua-do-monte,2.4,4).
aresta(real/rua-dos-paiois,palmeira/rua-do-rio,8.5,12).
aresta(maximinos/rua-de-caires,palmeira/rua-da-poca,6.5,9).
aresta(maximinos/rua-da-naia,palmeira/rua-do-monte,7,10).
aresta(maximinos/rua-de-caires,maximinos/rua-da-naia,2.2,5).
aresta(maximinos/rua-do-cruzeiro,maximinos/rua-da-naia,0.65,2).
aresta(maximinos/rua-de-caires,maximinos/rua-do-cruzeiro,2,4).
aresta(real/rua-do-tojal,maximinos/rua-do-cruzeiro,1.8,4).
aresta(real/rua-dos-paiois,real/rua-do-tojal,0.4,2).
aresta(real/rua-das-giestas,real/rua-do-tojal,0.3,2).
aresta(real/rua-das-giestas,real/rua-dos-paiois,0.35,1).
aresta(sao-vitor/rua-da-fabrica,sao-vitor/rua-do-taxa,1.2,4).
aresta(sao-vitor/rua-dom-pedro-v,sao-vitor/rua-da-fabrica,0.8,2).
aresta(sao-vitor/rua-dom-pedro-v,sao-vitor/rua-do-taxa,1.6,3).
aresta(nogueiro/rua-da-capela,sao-vitor/rua-da-fabrica,3.1,6).
aresta(gualtar/rua-do-fontelo,sao-vitor/rua-dom-pedro-v,2.7,5).
aresta(lamacaes/rua-da-torre,sao-vitor/rua-do-taxa,3.9,8).
aresta(gualtar/rua-do-fontao,gualtar/rua-do-fontelo,2.3,5).
aresta(gualtar/green-distribution,gualtar/rua-do-fontao,2.9,6).
aresta(gualtar/rua-breias,gualtar/green-distribution,2.4,5).
aresta(nogueiro/rua-do-major,gualtar/rua-breias,5,9).
aresta(nogueiro/rua-da-rasa,nogueiro/rua-do-major,0.4,2).
aresta(nogueiro/rua-da-capela,nogueiro/rua-da-rasa,0.75,3).
aresta(nogueiro/rua-do-major,nogueiro/rua-da-capela,0.65,2).
aresta(lamacaes/rua-da-carreira,gualtar/rua-do-fontao,4.9,9).
aresta(lamacaes/rua-da-torre,lamacaes/rua-da-carreira,0.7,2).
aresta(lamacaes/rua-do-passal,lamacaes/rua-da-torre,0.17,1).
aresta(nogueiro/rua-da-rasa,lamacaes/rua-do-passal,1.6,4).
aresta(real/rua-das-giestas,sao-vitor/rua-do-taxa,6.3,9).


+estima(Inicio, _, _) :: (findall(Inicio,(estima(Inicio, _, _)),Sol), length(Sol,N), N < 1).

estima(gualtar/green-distribution,0,0).
estima(gualtar/rua-breias,2,4.5).
estima(gualtar/rua-do-fontao,2.9,5.8).
estima(gualtar/rua-do-fontelo,5.2,12).
estima(lamacaes/rua-da-carreira,7.8,16.7).
estima(lamacaes/rua-da-torre,8.5,16.4).
estima(lamacaes/rua-do-passal,8.7,17.5).
estima(nogueiro/rua-do-major,7.4,13).
estima(nogueiro/rua-da-capela,8.1,15).
estima(nogueiro/rua-do-rasa,7.8,16).
estima(sao-vitor/rua-dom-pedro-v,7.9,14.8).
estima(sao-vitor/rua-da-fabrica,6.6,18).
estima(sao-vitor/rua-do-taxa,9.5,20).
estima(real/rua-das-giestas,15.8,25).
estima(real/rua-dos-paiois,16.1,29).
estima(real/rua-do-tojal,16.1,28). 
estima(palmeira/rua-do-rio,24.7,40).
estima(palmeira/rua-da-poca,28.1,47).
estima(palmeira/rua-do-monte,27.8,43).
estima(maximinos/rua-do-cruzeiro,17.9,32).
estima(maximinos/rua-de-caires,19.9,35).
estima(maximinos/rua-da-naia,18.6,30).
estima(gualtar/green-distribution,0,0).



%circuito(IdEntrega,Caminho)

+circuito(Id, _) :: (findall(Id,(circuito(Id,_)),Sol), length(Sol,N), N < 1).


circuito(televisao, [real/rua-dos-paiois, real/rua-das-giestas, sao-vitor/rua-do-taxa, sao-vitor/rua-dom-pedro-v, gualtar/rua-do-fontelo, gualtar/rua-do-fontao, gualtar/green-distribution]).
circuito(portatil, [nogueiro/rua-do-major, gualtar/rua-breias, gualtar/green-distribution]).
circuito(telemovel, [nogueiro/rua-da-capela, nogueiro/rua-do-major, gualtar/rua-breias, gualtar/green-distribution]).
circuito(forno, [nogueiro/rua-do-major, gualtar/rua-breias, nogueiro/rua-da-capela, gualtar/green-distribution]).
circuito(rato, [sao-vitor/rua-do-taxa, nogueiro/rua-da-capela, real/rua-dos-paiois, nogueiro/rua-do-major, gualtar/rua-breias, gualtar/green-distribution]).
circuito(pizza, [sao-vitor/rua-dom-pedro-v, nogueiro/rua-da-capela, nogueiro/rua-do-major, gualtar/rua-breias, gualtar/green-distribution]).



% encomenda(Freguesia/Rua,idEncomenda,idCliente, DataPrazo,TimePrazo, peso/volume, preco).


+encomenda(Id, _, _, _, _, _, _) :: (findall(Id,(encomenda(_,Id,_, _, _, _, _)),Sol), length(Sol,N), N < 1).


encomenda(palmeira/rua-do-rio, lataDaMonster, yoda, data(2021, 01, 05), hora(15,40), 10/2, 50).   %deixei a hora em separado pq na entrega as datas estao como um tuplo
encomenda(real/rua-das-giestas, francesinha, darthMaul, data(2022, 01, 05), hora(10,30), 5/8, 10).
encomenda(nogueiro/rua-da-capela, casaco, darthVader, data(2021, 04, 19), hora(22,21), 20/1, 60).
encomenda(gualtar/rua-breias, mala, jangoFett, data(2021, 10, 11), hora(12,30), 10/4, 5).
encomenda(sao-vitor/rua-dom-pedro-v, bicicleta, mandalorian, data(2020, 11, 03), hora(20,10), 9/2, 2000).
encomenda(maximinos/rua-do-cruzeiro, sapatos, stormtrooper, data(2022, 01, 04), hora(22,05), 1/1, 99).

encomenda(palmeira/rua-do-rio, lata, yoda,data(2021, 02, 05), hora(15,40), 10/2, 50).




% entrega(idEncomenda, idEstafeta, idCliente, freguesia/rua , dataMax/dataEntrega , classificação, peso/volume, preço)

+entrega(_, Id, _, _, _, _, _, _) :: (findall(Id,(entrega(Id, _, _, _, _, _, _, _)),Sol), length(Sol,N), N < 1).


entrega(televisao, darthMaul, manuel, palmeira/rua-do-rio, data(2021, 1, 30)/data(2021, 1, 29),5, 30/80, 10).
entrega(portatil, rui, bernardo, real/rua-das-giestas, data(2021, 2, 12)/data(2021, 2, 11),4 , 12/30, 5).
entrega(telemovel, ze-joao, miguel, nogueiro/rua-da-capela, data(2021, 3, 10)/data(2021, 1, 29), 3, 3/10, 3).

entrega(forno, darthVader, bernardo, gualtar/rua-breias, data(2021, 2, 12)/data(2021, 3, 29), 1, 12/30, 5).
entrega(teclado, jangoFett, alberto, maximinos/rua-do-cruzeiro, data(2021, 6, 19)/data(2021, 5, 22), 5, 21/30, 4).
entrega(rato, r2d2, joao, real/rua-dos-paiois, data(2021, 6, 22)/data(2021, 6, 22), 5, 2/30, 50).
entrega(headset, c3po, ana, lamacaes/rua-do-passal, data(2021, 12, 30)/data(2021, 12, 29), 3, 9/30, 24).

entrega(pao, gigachad, ana, maximinos/rua-da-naia,data(2021, 3, 10)/data(2021, 1, 29), 3, 3/10, 3).
entrega(pizza, leia, bernardo, maximinos/rua-de-caires,data(2021, 3, 11)/data(2021, 1, 30), 3, 3/10, 3).
entrega(hamburger, luke, antonio, sao-vitor/rua-da-fabrica, data(2021, 3, 30)/data(2021,3,15), 5 , 4/11 , 5).

entrega(lataDaMonster, yoda, palmeira/rua-do-rio, data(2021, 01, 05),data(2021, 01, 05) , 5, 10/2, 50). 
entrega(lata, yoda, palmeira/rua-do-rio, data(2021, 01, 05),data(2021, 01, 05) , 5, 10/2, 50).  




% estafeta(idEstafeta, freguesia, veiculo)

+estafeta(Id, _, _) :: (findall(Id,(estafeta(Id, _, _)),Sol), length(Sol,N), N < 1).

estafeta(bobbaFett, palmeira, carro).
estafeta(yoda, palmeira, mota).
estafeta(stormtrooper, palmeira, bicicleta).

estafeta(jangoFett, maximinos, carro).
estafeta(darthVader, maximinos, mota).
estafeta(luke, maximinos, bicicleta).

estafeta(r2d2, real, carro).
estafeta(rui, real, mota).
estafeta(leia, real, bicicleta).

estafeta(anakin, sao-vitor, carro).
estafeta(chewbacca, sao-vitor, mota).
estafeta(ze-joao, sao-vitor, bicicleta).

estafeta(c3po, gualtar, carro).
estafeta(bb8, gualtar, mota).
estafeta(miguel, gualtar, bicicleta).

estafeta(hanSolo, nogueiro, carro).
estafeta(margarida, nogueiro, mota).
estafeta(mandalorian, nogueiro, bicicleta).

estafeta(maceWindu, lamacaes, carro).
estafeta(darthMaul, lamacaes, mota).
estafeta(gigachad, lamacaes, bicicleta).



% veiculo (tipo de veiculo, velocidade_media, cargaMax, vertente_ecologica)
veiculo(bicicleta, 10, 5, 50).
veiculo(mota, 35, 20, 30).
veiculo(carro, 25, 100, 10). 



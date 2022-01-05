:- op( 900,xfy,'::' ).

:- dynamic encomenda/7.
:- dynamic circuito/2.
:- dynamic estafeta/3.
:- dynamic entrega/9.


%grafo

+aresta(Inicio, Fim, _, _) :: (findall((Inicio,Fim),(aresta(Inicio,Fim, _, _)),Sol), length(Sol,N), N == 1).

aresta(palmeira/rua_do_rio,palmeira/rua_da_poca,3.4,6).
aresta(palmeira/rua_do_rio,palmeira/rua_do_monte,3.1,5).
aresta(palmeira/rua_do_monte,palmeira/rua_da_poca,2.4,4).
aresta(palmeira/rua_do_rio,real/rua_dos_paiois,8.5,12).
aresta(palmeira/rua_da_poca,maximinos/rua_do_caires,6.5,9).
aresta(palmeira/rua_do_monte,maximinos/rua_da_naia,7,10).
aresta(maximinos/rua_da_naia,maximinos/rua_de_caires,2.2,5).
aresta(maximinos/rua_da_naia,maximinos/rua_do_cruzeiro,0.65,2).
aresta(maximinos/rua_do_cruzeiro,maximinos/rua_de_caires,2,4).
aresta(maximinos/rua_do_cruzeiro,real/rua_do_tojal,1.8,4).
aresta(real/rua_do_tojal,real/rua_dos_paiois,0.4,2).
aresta(real/rua_do_tojal,real/rua_das_giestas,0.3,2).
aresta(real/rua_dos_paiois,real/rua_das_giestas,0.35,1).
aresta(sao_vitor/rua_do_taxa,real/rua_das_giestas,6.3,9).
aresta(sao_vitor/rua_do_taxa,sao_vitor/rua_da_fabrica,1.2,4).
aresta(sao_vitor/rua_da_fabrica,sao_vitor/rua_dom_pedro_v,0.8,2).
aresta(sao_vitor/rua_do_taxa,sao_vitor/rua_dom_pedro_v,1.6,3).
aresta(sao_vitor/rua_da_fabrica,nogueiro/rua_da_capela,3.1,6).
aresta(sao_vitor/rua_dom_pedro_v,gualtar/rua_do_fontelo,2.7,5).
aresta(sao_vitor/rua_do_taxa,lamacaes/rua_da_torre,3.9,8).
aresta(gualtar/rua_do_fontelo,gualtar/rua_do_fontao,2.3,5).
aresta(gualtar/rua_do_fontao,gualtar/green_distribution,2.9,6).
aresta(gualtar/green_distribution,gualtar/rua_breias,2.4,5).
aresta(gualtar/rua_breias,nogueiro/rua_do_major,5,9).
aresta(nogueiro/rua_do_major,nogueiro/rua_da_rasa,0.4,2).
aresta(nogueiro/rua_da_rasa,nogueiro/rua_da_capela,0.75,3).
aresta(nogueiro/rua_da_capela,nogueiro/rua_do_major,0.65,2).
aresta(gualtar/rua_do_fontao,lamacaes/rua_da_carreira,4.9,9).
aresta(lamacaes/rua_da_carreira,lamacaes/rua_da_torre,0.7,2).
aresta(lamacaes/rua_da_torre,lamacaes/rua_do_passal,0.17,1).
aresta(lamacaes/rua_do_passal,nogueiro/rua_da_rasa,1.6,4).

%sentido inverso 
aresta(palmeira/rua_da_poca,palmeira/rua_do_rio,3.4,6).
aresta(palmeira/rua_do_monte,palmeira/rua_do_rio,3.1,5).
aresta(palmeira/rua_da_poca,palmeira/rua_do_monte,2.4,4).
aresta(real/rua_dos_paiois,palmeira/rua_do_rio,8.5,12).
aresta(maximinos/rua_de_caires,palmeira/rua_da_poca,6.5,9).
aresta(maximinos/rua_da_naia,palmeira/rua_do_monte,7,10).
aresta(maximinos/rua_de_caires,maximinos/rua_da_naia,2.2,5).
aresta(maximinos/rua_do_cruzeiro,maximinos/rua_da_naia,0.65,2).
aresta(maximinos/rua_de_caires,maximinos/rua_do_cruzeiro,2,4).
aresta(real/rua_do_tojal,maximinos/rua_do_cruzeiro,1.8,4).
aresta(real/rua_dos_paiois,real/rua_do_tojal,0.4,2).
aresta(real/rua_das_giestas,real/rua_do_tojal,0.3,2).
aresta(real/rua_das_giestas,real/rua_dos_paiois,0.35,1).
aresta(sao_vitor/rua_da_fabrica,sao_vitor/rua_do_taxa,1.2,4).
aresta(sao_vitor/rua_dom_pedro_v,sao_vitor/rua_da_fabrica,0.8,2).
aresta(sao_vitor/rua_dom_pedro_v,sao_vitor/rua_do_taxa,1.6,3).
aresta(nogueiro/rua_da_capela,sao_vitor/rua_da_fabrica,3.1,6).
aresta(gualtar/rua_do_fontelo,sao_vitor/rua_dom_pedro_v,2.7,5).
aresta(lamacaes/rua_da_torre,sao_vitor/rua_do_taxa,3.9,8).
aresta(gualtar/rua_do_fontao,gualtar/rua_do_fontelo,2.3,5).
aresta(gualtar/green_distribution,gualtar/rua_do_fontao,2.9,6).
aresta(gualtar/rua_breias,gualtar/green_distribution,2.4,5).
aresta(nogueiro/rua_do_major,gualtar/rua_breias,5,9).
aresta(nogueiro/rua_da_rasa,nogueiro/rua_do_major,0.4,2).
aresta(nogueiro/rua_da_capela,nogueiro/rua_da_rasa,0.75,3).
aresta(nogueiro/rua_do_major,nogueiro/rua_da_capela,0.65,2).
aresta(lamacaes/rua_da_carreira,gualtar/rua_do_fontao,4.9,9).
aresta(lamacaes/rua_da_torre,lamacaes/rua_da_carreira,0.7,2).
aresta(lamacaes/rua_do_passal,lamacaes/rua_da_torre,0.17,1).
aresta(nogueiro/rua_da_rasa,lamacaes/rua_do_passal,1.6,4).
aresta(real/rua_das_giestas,sao_vitor/rua_do_taxa,6.3,9).


+estima(Inicio, _, _) :: (findall(Inicio,(estima(Inicio, _, _)),Sol), length(Sol,N), N == 1).

estima(gualtar/green_distribution,0,0).
estima(gualtar/rua_breias,2,4.5).
estima(gualtar/rua_do_fontao,2.9,5.8).
estima(gualtar/rua_do_fontelo,5.2,12).
estima(lamacaes/rua_da_carreira,7.8,16.7).
estima(lamacaes/rua_da_torre,8.5,16.4).
estima(lamacaes/rua_do_passal,8.7,17.5).
estima(nogueiro/rua_do_major,7.4,13).
estima(nogueiro/rua_da_capela,8.1,15).
estima(nogueiro/rua_do_rasa,7.8,16).
estima(sao_vitor/rua_dom_pedro_v,7.9,14.8).
estima(sao_vitor/rua_da_fabrica,6.6,18).
estima(sao_vitor/rua_do_taxa,9.5,20).
estima(real/rua_das_giestas,15.8,25).
estima(real/rua_dos_paiois,16.1,29).
estima(real/rua_do_tojal,16.1,28). 
estima(palmeira/rua_do_rio,24.7,40).
estima(palmeira/rua_da_poca,28.1,47).
estima(palmeira/rua_do_monte,27.8,43).
estima(maximinos/rua_do_cruzeiro,17.9,32).
estima(maximinos/rua_de_caires,19.9,35).
estima(maximinos/rua_da_naia,18.6,30).
estima(gualtar/green_distribution,0,0).



%circuito(IdEntrega,Caminho)

+circuito(Id, _) :: (findall(Id,(circuito(Id,_)),Sol), length(Sol,N), N == 1).


circuito(televisao, [real/rua_dos_paiois, real/rua_das_giestas, sao_vitor/rua_do_taxa, sao_vitor/rua_dom_pedro_v, gualtar/rua_do_fontelo, gualtar/rua_do_fontao, gualtar/green_distribution]).
circuito(portatil, [nogueiro/rua_do_major, gualtar/rua_breias, gualtar/green_distribution]).
circuito(telemovel, [nogueiro/rua_da_capela, nogueiro/rua_do_major, gualtar/rua_breias, gualtar/green_distribution]).
circuito(forno, [nogueiro/rua_do_major, nogueiro/rua_da_rasa, lamacaes/rua_do_passal, lamacaes/rua_da_torre, sao_vitor/rua_do_taxa, sao_vitor/rua_dom_pedro_v,gualtar/rua_do_fontelo, gualtar/rua_do_fontao, gualtar/green_distribution]).
circuito(rato, [maximinos/rua_do_cruzeiro, real/rua_do_tojal, real/rua_das_giestas, sao_vitor/rua_do_taxa, sao_vitor/rua_dom_pedro_v, gualtar/rua_do_fontelo, gualtar/rua_do_fontao, gualtar/green_distribution]).
circuito(pizza, [palmeira/rua_do_rio, real/rua_dos_paiois, real/rua_das_giestas, sao_vitor/rua_do_taxa, sao_vitor/rua_da_fabrica, nogueiro/rua_da_capela, nogueiro/rua_do_major, gualtar/rua_breias, gualtar/green_distribution]).



% encomenda(Freguesia/Rua,idEncomenda,idCliente, DataPrazo,TimePrazo, peso/volume, preco).

+encomenda(_, Id, _, _, _, _, _) :: (findall(Id,(encomenda(_,Id,_, _, _, _, _)),Sol), length(Sol,N), N == 1).


encomenda(palmeira/rua_do_rio, lataDaMonster, yoda, data(2021, 01, 05), hora(15,40), 10/2, 50).   %deixei a hora em separado pq na entrega as datas estao como um tuplo
encomenda(real/rua_das_giestas, francesinha, darthMaul, data(2022, 01, 05), hora(10,30), 5/8, 10).
encomenda(nogueiro/rua_da_capela, casaco, darthVader, data(2021, 04, 19), hora(22,21), 20/1, 60).
encomenda(gualtar/rua_breias, mala, jangoFett, data(2021, 10, 11), hora(12,30), 10/4, 5).
encomenda(sao_vitor/rua_dom_pedro_v, bicicleta, mandalorian, data(2020, 11, 03), hora(20,10), 9/2, 2000).
encomenda(maximinos/rua_do_cruzeiro, sapatos, stormtrooper, data(2022, 01, 04), hora(22,05), 1/1, 99).

encomenda(palmeira/rua_do_rio, pizza, yoda, data(2021, 02, 05), hora(15,40), 10/2, 50).


% entrega(idEncomenda, idEstafeta, idCliente, freguesia/rua , dataPrazo/horaPrazo,dataEntrega/horaEntrega , classificação, peso/volume, preço)

+entrega(Id, _, _, _, _, _, _, _) :: (findall(Id,(entrega(Id, _, _, _, _, _, _, _)),Sol), length(Sol,N), N == 1).


entrega(televisao, darthMaul, manuel, palmeira/rua_do_rio, data(2021, 1, 30)/hora(12,00),data(2021, 1, 29)/hora(12,00),5, 30/80, 10).
entrega(portatil, rui, bernardo, real/rua_das_giestas, data(2021, 2, 12)/hora(12,00),data(2021, 2, 11)/hora(12,00),4 , 12/30, 5).
entrega(telemovel, ze_joao, miguel, nogueiro/rua_da_capela, data(2021, 3, 10)/hora(12,00),data(2021, 1, 29)/hora(12,00), 3, 3/10, 3).

entrega(forno, darthVader, bernardo, gualtar/rua_breias, data(2021, 2, 12)/hora(12,00),data(2021, 3, 29)/hora(12,00), 1, 12/30, 5).
entrega(teclado, jangoFett, alberto, maximinos/rua_do_cruzeiro, data(2021, 6, 19)/hora(12,00),data(2021, 5, 22)/hora(12,00), 5, 21/30, 4).
entrega(rato, r2d2, joao, real/rua_dos_paiois, data(2021, 6, 22)/hora(12,00),data(2021, 6, 22)/hora(12,00), 5, 2/30, 50).
entrega(headset, c3po, ana, lamacaes/rua_do_passal, data(2021, 12, 30)/hora(12,00),data(2021, 12, 29)/hora(12,00), 3, 9/30, 24).

entrega(pao, gigachad, ana, maximinos/rua_da_naia,data(2021, 3, 10)/hora(12,00),data(2021, 1, 29)/hora(12,00), 3, 3/10, 3).
entrega(pizza, leia, bernardo, maximinos/rua_de_caires,data(2021, 3, 11)/hora(12,00),data(2021, 1, 30)/hora(12,00), 3, 3/10, 3).
entrega(hamburger, luke, antonio, sao_vitor/rua_da_fabrica, data(2021, 3, 30)/hora(12,00),data(2021,3,15)/hora(12,00), 5 , 4/11 , 5).

entrega(lataDaMonster, yoda, manuel, palmeira/rua_do_rio, data(2021, 01, 06)/hora(12,00),data(2021, 01, 05)/hora(12,00) , 5, 10/2, 50). 
entrega(lata, yoda, ze, palmeira/rua_do_rio, data(2021, 01, 07)/hora(12,00),data(2021, 01, 05)/hora(12,00) , 5, 10/2, 50).  
entrega(cogumelos, margarida,joao, palmeira/rua_do_rio,  data(2021, 01, 05)/hora(15,40),data(2021, 04, 18)/hora(22,00),5, 10/2, 50).




% estafeta(idEstafeta, freguesia, veiculo)

+estafeta(Id, _, _) :: (findall(Id,(estafeta(Id, _, _)),Sol), length(Sol,N), N == 1).

estafeta(bobbaFett, palmeira, carro).
estafeta(yoda, palmeira, mota).
estafeta(stormtrooper, palmeira, bicicleta).

estafeta(jangoFett, maximinos, carro).
estafeta(darthVader, maximinos, mota).
estafeta(luke, maximinos, bicicleta).

estafeta(r2d2, real, carro).
estafeta(rui, real, mota).
estafeta(leia, real, bicicleta).

estafeta(anakin, sao_vitor, carro).
estafeta(chewbacca, sao_vitor, mota).
estafeta(ze_joao, sao_vitor, bicicleta).

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


%transporte (idEncomenda, DataInicio/HoraInicio, DataFim/HoraFim)

transporte(IdEncomenda, DataInicio/HoraInicio, DataFim/HoraFim).
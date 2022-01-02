:- op( 900,xfy,'::' ).


+entrega(Id, _, _, _, _, _, _, _) :: ((findall(Id,(entrega(Id, _, _, _, _, _, _, _)),Sol), length(Sol,N), N < 2)).


% entrega(identrega, idEstafeta, idCliente, freguesia/rua , dataMax/dataEntrega , classificação, peso/volume, preço)
entrega(televisao, joaquim, manuel, vila-caiz/aldeia-nova, data(2021, 1, 30)/data(2021, 1, 29),5, 30/80, 10).
entrega(portatil, rui, bernardo, roriz/pidre, data(2021, 2, 12)/data(2021, 2, 11),4 , 12/30, 5).
entrega(telemovel, ze-joao, miguel, barcelos/pedreira, data(2021, 3, 10)/data(2021, 1, 29), 3, 3/10, 3).

entrega(forno, painatal, bernardo, roriz/pidre, data(2021, 2, 12)/data(2021, 3, 29), 1, 12/30, 5).
entrega(telemovel, margarida, pedro, vila-caiz/aldeia-nova, data(2021, 3, 10)/data(2021, 3, 9), 2, 3/10, 3).
entrega(teclado, rui, alberto, esposende/margem, data(2021, 6, 19)/data(2021, 5, 22), 5, 21/30, 4).
entrega(rato, miguel, joao, esposende/margem, data(2021, 6, 22)/data(2021, 6, 22), 5, 2/30, 50).
entrega(headset, margarida, ana, esposende/margem, data(2021, 12, 30)/data(2021, 12, 29), 3, 9/30, 24).

entrega(pao, gigachad, ana, braga/vila-verde,data(2021, 3, 10)/data(2021, 1, 29), 3, 3/10, 3).
entrega(pizza, gigachad, bernardo, braga/vila-verde,data(2021, 3, 11)/data(2021, 1, 30), 3, 3/10, 3).
entrega(hamburger, gigachad, antonio, braga/real, data(2021, 3, 30)/data(2021,3,15), 5 , 4/11 , 5).
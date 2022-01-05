:-include('algoritmos.pl').

:- op(900,xfy,'::').

:- dynamic encomenda/7.
:- dynamic circuito/2.
:- dynamic estafeta/3.
:- dynamic entrega/9.

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
contaEntregas(Res) :- findall(IdEntrega, entrega(IdEntrega, _, _, _, _, _, _, _, _), Sol), length(Sol, Res).


mostraEntregas(Res) :-
    findall(IdEntrega, entrega(IdEntrega, _, _, _, _, _, _, _, _), Sol),
    length(Sol,Res).


% Circuitos com mais entregas
% recebe x para mostrar top x caminhos
% recebe Solucao onde irá armazenar a lista

devolveCusto(Lista/Custo,Custo).





descending([], []).
descending([A], [A]).
descending(A,  [X,Y|C]) :-
  select(X, A, B),
  descending(B, [Y|C]),
        devolveCusto(X,W),
        devolveCusto(Y,Z),
          W   >=    Z.


circuitosComMaisEntregas(NumCircuitos, Solucao) :-
    findall(Caminho, circuito(_,Caminho),Caminhos),
    circuitosComMaisEntregasAux(Caminhos,[],Res),
    descending(Res,ResOrder),
    take(NumCircuitos,ResOrder,Solucao).
   

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




indicadorDeProdutividade(circuito(IdEntrega, Caminho), Res) :-
    calculaCusto(Caminho, TotalDist),
    calculaTempo(Caminho, TotalTempo),
    getVeiculo(IdEntrega, Veiculo),
    veiculo(Veiculo, _, _, VertenteEco),
    Res is (TotalDist+TotalTempo) * VertenteEco.


getCaminho(Circuito, ResCaminho) :- circuito(_, ResCaminho).

getVeiculo(IdEntrega, ResVeiculo) :-
    entrega(IdEntrega, IdEstafeta, _, _, _, _, _, _,_),
    estafeta(IdEstafeta, _, ResVeiculo).



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
                         round((Distancia/NovaVelocidadeMedia) * 60,Tempo);
    Veiculo == mota -> decrescimo_motos(35, Peso, NovaVelocidadeMedia),
                          round((Distancia/NovaVelocidadeMedia) * 60,Tempo);
    Veiculo == bicicleta -> decrescimo_bicicleta(10, Peso, NovaVelocidadeMedia),
                         round((Distancia/NovaVelocidadeMedia) * 60,Tempo);
    !, fail.


descendingEco([], []).
descendingEco([A], [A]).
descendingEco(A,  [X,Y|C]) :-
  select(X, A, B),
  descendingEco(B, [Y|C]),
        estafeta(X,_,Veiculo1),
        veiculo(Veiculo1,_,_,W),
        estafeta(Y,_,Veiculo2),
        veiculo(Veiculo2,_,_,Z),
          W   >=    Z.


%----- Devolve Encomendas que ainda não foram entregues ----------------------------------------------------------------------

encomendasPorEntregar(Encomendas) :-
        findall(IdEncomenda,encomenda(_,IdEncomenda, _, _, _, _, _),Todas),
        retiraEntregues(Todas,Encomendas).


retiraEntregues([],[]).

retiraEntregues([IdEncomenda|Encomendas],Res):-
    findall(Id,entrega(Id, _, _, _, _, _, _, _, _),Entregas),
    member(IdEncomenda,Entregas),
    retiraEntregues(Encomendas,Res).

retiraEntregues([IdEncomenda|Encomendas],[IdEncomenda|Res]):-
    findall(Id,entrega(Id, _, _, _, _, _, _, _, _),Entregas),
    not(member(IdEncomenda,Entregas)),
    retiraEntregues(Encomendas,Res).


%------------------------------------ Circuito mais rápido ---------------------------------------------------------------------



verificaListaEstafeta(_,[], nop).

verificaListaEstafeta(Data/Hora,[IdEstafeta|Ids], IdEstafeta):-
    verificaDisponiblidadeEstafeta(Data/Hora,IdEstafeta),!.

verificaListaEstafeta(Data/Hora,[IdEstafeta|Ids], Res):-
    not(verificaDisponiblidadeEstafeta(Data/Hora,IdEstafeta)),
    verificaListaEstafeta(Data/Hora,Ids,Res),!.


verificaDisponiblidadeEstafeta(Data/Hora,Id) :-
    findall(IdEncomenda,entrega(IdEncomenda, Id, _, _ , _, _, _, _,_),Lista),
    verificaDisponiblidadeEstafetaAux(Data/Hora,Lista).


verificaDisponiblidadeEstafetaAux(Data/Hora,[]).


verificaDisponiblidadeEstafetaAux(Data/Hora,[IdEncomenda|IdEncomendas]):-
    entrega(IdEncomenda, _, _, _, _,DataEntrega/HoraEntrega , _, _, _),
    compare_data(Data, =, DataEntrega),
    compare_hora(Hora, >, HoraEntrega),
    verificaDisponiblidadeEstafetaAux(Data/Hora,IdEncomendas).

verificaDisponiblidadeEstafetaAux(Data/Hora,[IdEncomenda|IdEncomendas]):-
    entrega(IdEncomenda, _, _, _, _,DataEntrega/HoraEntrega , _, _, _),
    not(compare_data(Data, =, DataEntrega)),
    verificaDisponiblidadeEstafetaAux(Data/Hora,IdEncomendas).




veiculo(bicicleta, 10, 5, 50).
veiculo(mota, 35, 20, 30).
veiculo(carro, 25, 100, 10). 






%-----------------------------------Devolve Estafeta mais Rapido disponivel-----------------------------------------------------

devolveMelhorEstafetaRapidez(Data/Hora,Zona,Peso,IdEstafeta):-
    Peso =< 100,
    writeln(Peso),
    findall(IdEstafeta,estafeta(IdEstafeta,Zona,carro),Carro),
    verificaListaEstafeta(Data/Hora,Carro,IdEstafeta),
    IdEstafeta \= nop,
    !.

devolveMelhorEstafetaRapidez(Data/Hora,Zona,Peso,IdEstafeta):-
    Peso =< 20,
    findall(IdEstafeta,estafeta(IdEstafeta,Zona,mota),Mota),
    verificaListaEstafeta(Data/Hora,Mota,IdEstafeta),
    IdEstafeta \= nop,
    !.


devolveMelhorEstafetaRapidez(Data/Hora,Zona, Peso,IdEstafeta):-
    Peso =< 10,
    findall(IdEstafeta,estafeta(IdEstafeta,Zona,bicicleta),Bicicleta),
    verificaListaEstafeta(Data/Hora,Bicicleta,IdEstafeta),
    !.
    
    

%-------------------------------------------------------------------------------------------------------------------------------------



escolherCircuitoMaisRapido(DataInicio/HoraInicio,IdEncomenda) :-
    encomenda(Zona/Rua,IdEncomenda,IdCliente,DataPrazo,HoraPrazo, Peso/Volume, Preco),
    devolveMelhorEstafetaRapidez(DataInicio/HoraInicio,Zona,Peso,IdEstafeta),
    IdEstafeta \= nop,
    write("O estafeta selecionado foi "),writeln(IdEstafeta),writeln(""),writeln(""),
    escolheAlgoritmo(1,Zona/Rua,Caminho/Distancia),
    writeln(Caminho),writeln(""),writeln(""),
    estafeta(IdEstafeta, _, Veiculo),
    calcularTempo(Distancia,Veiculo,Peso,Tempo),
    write("O Tempo para a entrega foi de "),write(Tempo),write(" minutos"),writeln(""),writeln(""),
    somaDataHora(DataInicio,HoraInicio,Tempo,DataEntrega/HoraEntrega),
    writeln("Introduza a classificacao da entrega"),
    read(Classificacao),
    evolucao(entrega(IdEncomenda,IdEstafeta,IdCliente,Zona/Rua,DataPrazo/HoraPrazo,DataEntrega/HoraEntrega,Classificacao,Peso/Volume,Preco)),
    evolucao(circuito(IdEncomenda,Caminho)).
    
   
    
%------------------------------------ Circuito mais Ecologico ---------------------------------------------------------------------

escolherCircuitoMaisEcologico(DataInicio/HoraInicio,IdEncomenda) :- 
        encomenda(Freguesia/Rua,IdEncomenda,IdCliente,DataPrazo,HoraPrazo, Peso/Volume, Preco),
        resolve_aestrelaD(Freguesia/Rua,CaminhoAux/DistanciaAux),
        duplicaCaminho(CaminhoAux/DistanciaAux,Caminho/Distancia), % algoritmo aestrela tendo em conta distancia
        write("Caminho a estrela: "),writeln(Caminho),
        atribuiEstafetaEco(Freguesia,Distancia,Peso,DataInicio/HoraInicio,DataPrazo/HoraPrazo,IdEstafetaAtri/Veiculo), % temos idestafeta e veiculo
        calcularTempo(Distancia,Veiculo,Peso,Tempo),
        write("Distancia"),writeln(Distancia),
        write("Tempo:"),writeln(Tempo),
        somaDataHora(DataInicio,HoraInicio,Tempo,DataEntrega/HoraEntrega),
        writeln("Introduza a classificacao da entrega"),
        read(Classificacao),
        evolucao(entrega(IdEncomenda,IdEstafetaAtri,IdCliente,Freguesia/Rua,DataPrazo/HoraPrazo,DataEntrega/HoraEntrega,Classificacao,Peso/Volume,Preco)),
        evolucao(circuito(IdEncomenda,Caminho)).

    


atribuiEstafetaEco(Freguesia,Distancia,Peso,DataInicio/HoraInicio,DataPrazo/HoraPrazo,IdEstafetaAtri/Veiculo) :- 
            veiculosPossiveisPeso(Peso,VeiculosPossiveisPeso),
            veiculosPossiveisPrazo(Peso,Distancia,DataInicio/HoraInicio,DataPrazo/HoraPrazo,VeiculosPossiveisPrazo),
            write("Peso: "),writeln(Peso),
            write("Vei possiveis peso"),writeln(VeiculosPossiveisPeso),
            write("Vei possiveis prazo"),writeln(VeiculosPossiveisPrazo),
            conjuncaoListas(VeiculosPossiveisPeso,VeiculosPossiveisPrazo,Veiculos),
            writeln(Veiculos),
            write("Freguesia"),writeln(Freguesia),
            listaEstafetaVeiculos(Freguesia,Veiculos,[],Estafetas),
            write("Estafetas"),writeln(Estafetas),
            descendingEco(Estafetas,EstafetasOrd),
            verificaListaEstafeta(DataInicio/HoraInicio,EstafetasOrd,IdEstafetaAtri),
            write("Estafeta atribuido:"),writeln(IdEstafetaAtri),
            estafeta(IdEstafetaAtri,_,Veiculo).

listaEstafetaVeiculos(Freguesia,[],Estafetas,Estafetas).
listaEstafetaVeiculos(Freguesia,[V],Estafetas,Res) :-
    findall(IdEstafeta,estafeta(IdEstafeta,Freguesia,V),EstafetasV),
    append(Estafetas,EstafetasV,Res),
    !.
listaEstafetaVeiculos(Freguesia,[V|Vs],Estafetas,Res) :-
    findall(IdEstafeta,estafeta(IdEstafeta,Freguesia,V),EstafetasV),
    append(Estafetas,EstafetasV,ResAux),
    listaEstafetaVeiculos(Freguesia,Vs,ResAux,Res),
    !.



veiculosPossiveisPeso(Peso,Veiculos) :-
     ((Peso>0,Peso=<10)     -> Veiculos=[bicicleta,mota,carro];
     (Peso>10,Peso=<20)    -> Veiculos=[mota,carro];
     (Peso>20,Peso=<100)  -> Veiculos=[carro];
     Veiculos = []).

% se DataFim(Data prevista para entrega) for antes da Dataprazo -> Ans = 1 senao = 0
checkPrazo(DataFim/HoraFim,DataPrazo/HoraPrazo,1) :- 
    compare_data(DataFim,<,DataPrazo).

checkPrazo(DataFim/HoraFim,DataPrazo/HoraPrazo,1) :- 
     compare_data(DataFim,=,DataPrazo),
     compare_hora(HoraFim,<,HoraPrazo).              

checkPrazo(DataFim/HoraFim,DataPrazo/HoraPrazo,0) .

veiculosPossiveisPrazo(Peso,Distancia,DataInicio/HoraInicio,DataPrazo/HoraPrazo,Veiculos) :-
    calcularTempo(Distancia,bicicleta,Peso,TempoBicicleta),
    somaDataHora(DataInicio,HoraInicio,TempoBicicleta,DataEntregaB/HoraEntregaB),
    checkPrazo(DataEntregaB/HoraEntregaB,DataPrazo/HoraPrazo,AnsB),
    calcularTempo(Distancia,mota,Peso,TempoMota),
    somaDataHora(DataInicio,HoraInicio,TempoMota,DataEntregaM/HoraEntregaM),
    checkPrazo(DataEntregaM/HoraEntregaM,DataPrazo/HoraPrazo,AnsM), 
    veiculosauxiliar(AnsB,AnsM,Veiculos).

veiculosauxiliar(AnsB,AnsM,Veiculos) :-
    (AnsB == 1) -> Veiculos = [bicicleta,mota,carro];
    (AnsB == 0,AnsM == 1) -> Veiculos = [mota,carro];
    (AnsB == 0,AnsM == 0) -> Veiculos = [carro];
    !,Veiculos = [].

         
               
    

% Funções data e hora
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




minutosToDate(Tempo,data(0,0,DD)/hora(HH,MM)):-
        H is Tempo//60,
        MM is Tempo mod 60,
        DD is H // 24,
        HH is H mod 24. 


somaDataHora(data(YY,MMM,DD),hora(HH,MM),Tempo,data(Ano,Mes,Dias)/hora(Horas,Minutos)) :-
    minutosToDate(Tempo,data(YT,MMT,DT)/hora(HT,MT)),
    M is MT + MM,
    Minutos is M mod 60,
    HorasSobra is M // 60,
    Horas is (HorasSobra + HH + HT) mod 24,
    DiasSobra is (HorasSobra + HH + HT) // 24,
    Dias is (DiasSobra + DT + DD) mod 30,
    MesSobra is (DiasSobra + DT + DD) // 30,
    Mes is (MesSobra + MMM + MMT) mod 12,
    AnoSobra is (MesSobra + MMM + MMT) // 12,
    Ano is (AnoSobra + YT + YY).

   
      
conjuncaoListas([],L,[]).
conjuncaoListas(L, [], []).

conjuncaoListas([X|Xs], Lista, [X|Res]) :-
    member(X,Lista),
    conjuncaoListas(Xs,Lista,Res),!.

conjuncaoListas([X|Xs], Lista, Res) :-
    conjuncaoListas(Xs,Lista,Res),!.


pertence( X,[X|L]).
pertence( X,[Y|L]) :-
    X \= Y,
    pertence( X,L ).


mostraEntregas(Res) :-
    findall(IdEntrega, entrega(IdEntrega, _, _, _, _, _, _, _, _), Sol),
    writeln(Sol),
    length(Sol,Res).
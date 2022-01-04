:-include('queries2.pl').

%para correr faz-se:
% swipl main.pl
% menu.
% 10.    -> opcao de exemplo de uma query
% argumentoDaQuery.   -> escrever o argumento da query

menu :- repeat, nl, nl, nl,
	write('________________________________________________ Sistema de Recomendação de Circuitos de Entrega de Encomendas ______________________________________'), nl, nl,
	write(' 1 - Gerar os circuitos de entrega, caso existam, que cubram um determinado território'), nl,
	write(' 2 - Identificar quais os circuitos com maior número de entregas (por volume e peso)'), nl,
	write(' 3 - Comparar circuitos de entrega tendo em conta os indicadores de produtividade'), nl,
	write(' 4 - Escolher o circuito mais rápido (usando o critério da distância)'), nl,
	write(' 5 - Escolher o circuito mais ecológico (usando um critério de tempo)'), nl,
	write(' 6 - Gerar um circuito de entrega com algoritmo à escolha'), nl,
	write(' 7 - Escrever aqui a pergunta...'), nl,
	write(' 8 - Escrever aqui a pergunta...'), nl,
	write(' 9 - Escrever aqui a pergunta...'), nl,
	write(' 10- Escrever aqui a pergunta...'), nl,
	write(' 11- Sair.'), nl, nl,
    write('Digite a opção:'), nl,
	read(Choice), runQuery(Choice), menu.


% FALTA FAZER
runQuery(1) :- 
	write("Digite o território que quer analisar: "),
	read(Territorio).

runQuery(2) :- 
	write("Digite o número de circuitos que pretende: "),
	read(NumeroCircuitos),
	circuitosComMaisEntregas(NumeroCircuitos, Solucao),
	write(Solucao).


runQuery(3) :- 
	write("Digite o número de circuitos que prentende: "),
	read(NumCircuitos),
	circuitosComMaiorProdutividade(NumCircuitos, Sol),
	write(Sol).

% FALTA FAZER
runQuery(4) :- 
	write("A gerar o circuito mais rápido...").

% FALTA FAZER
runQuery(5) :- 
	write("A gerar o circuito mais ecológico...").

runQuery(6) :- 
	write("1 - A Estrela."), nl,
	write("2 - Gulosa com estima pela distancia."), nl,
	write("3 - Gulosa com estima pelo tempo."), nl,
	write("4 - Depth First Search."), nl,
	write("5 - Breadth First Search"), nl,
	write("6 - Busca Iterativa Limitada em Profundidade"), nl,
	read(NumeroAlg),
	write("Digite o nodo onde pretende levar a encomenda: "),
	read(Nodo),
	escolheAlgoritmo(NumeroAlg, Nodo, Circuito/NovoCusto),
	write(Circuito/NovoCusto).



runQuery(10) :- 
    write('Indique qual/quais a(s) Encomenda(s) que pretende analisar'),
    read(IdEncomenda), 
    write("Recebido o input: "), writeln(IdEncomenda).
    % query_123(IdEncomenda).



%predicado que termina o programa
runQuery(11) :- 
	writeln("Até breve!"), nl, halt.

%opcao final que so ocorre se nao entrar em nenhum dos casos anteriores
runQuery(_) :- 
	writeln("A opção não é válida!").

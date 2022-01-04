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
	write(' 6 - Escrever aqui a pergunta...'), nl,
	write(' 7 - Escrever aqui a pergunta...'), nl,
	write(' 8 - Escrever aqui a pergunta...'), nl,
	write(' 9 - Escrever aqui a pergunta...'), nl,
	write(' 10- Escrever aqui a pergunta...'), nl,
	write(' 11- Sair.'), nl, nl,
    write('Digite a opção:'), nl,
	read(Choice), runQuery(Choice), menu.




runQuery(10) :- 
    write('Indique qual/quais a(s) Encomenda(s) que pretende analisar'),
    read(IdEncomenda), 
    write("Recebido o input: "), writeln(IdEncomenda).
    % query_123(IdEncomenda).



%predicado que termina o programa
runQuery(11) :- writeln("Até breve!"), nl, halt.

%opcao final que so ocorre se nao entrar em nenhum dos casos anteriores
runQuery(_) :- writeln("A opção não é válida!").

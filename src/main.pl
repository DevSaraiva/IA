:-include('queries1.pl').

%para correr faz-se:
% swipl main.pl
% menu.
% 2.    -> opcao de exemplo de uma query
% argumentoDaQuery.   -> escrever o argumento da query

menu :- repeat, nl, nl, nl,
	write('________________________________________________ Sistema de Recomendação de Circuitos de Entrega de Encomendas ______________________________________'), nl, nl,
	write(' 1 - Escrever aqui a pergunta...'), nl,
	write(' 2 - Escrever aqui a pergunta...'), nl,
	write(' 3 - Escrever aqui a pergunta...'), nl,
	write(' 4 - Escrever aqui a pergunta...'), nl,
	write(' 5 - Escrever aqui a pergunta...'), nl,
	write(' 6 - Escrever aqui a pergunta...'), nl,
	write(' 7 - Escrever aqui a pergunta...'), nl,
	write(' 8 - Escrever aqui a pergunta...'), nl,
	write(' 9 - Escrever aqui a pergunta...'), nl,
	write(' 10- Escrever aqui a pergunta...'), nl,
	write(' 11- Sair.'), nl, nl,
    write('Digite a opção:'), nl,
	read(Choice), run_query(Choice), menu.




run_query(2) :- 
    write('Indique qual/quais a(s) Encomenda(s) que pretende analisar'),
    read(IdEncomenda), 
    write("Recebido o input: "), writeln(IdEncomenda).
    % query_123(IdEncomenda, IdEstafeta).



%predicado que termina o programa
run_query(11) :- writeln("Até breve!"), nl, halt.

%opcao final que so ocorre se nao entrar em nenhum dos casos anteriores
run_query(_) :- writeln("Escolha uma Opção Válida!").
:- initialization(main, main).

:- use_module(inicial).
:- use_module(auth).
:- use_module(menu).

:- use_module(jogo).
:- use_module(utils).
:- use_module(navegacao).
:- use_module(gerenciador_progresso).
:- use_module('dados/missoes').
:- use_module('dados/questoes').

main :-
    pagina_inicial,
    (   loop_autenticacao ->      % 1. Tenta autenticar
        menu_principal          % 2. Se conseguir, chama o menu
    ;   writeln('Encerrando o programa.')
    ).
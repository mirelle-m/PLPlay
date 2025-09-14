:- module(run, [run/0]).

:- use_module(auth).
:- use_module(menu).
:- use_module(utils).

run :-
    pagina_inicial,
    auth:loop_autenticacao ->
        menu:menu_principal;
        writeln('Encerrando o programa.').
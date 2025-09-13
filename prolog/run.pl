:- module(run, [run/0]).

:- use_module(auth).
:- use_module(menu).
% Add all your other use_module directives here.

run :-
    auth:loop_autenticacao ->
        menu:menu_principal
    ;
        writeln('Encerrando o programa.').
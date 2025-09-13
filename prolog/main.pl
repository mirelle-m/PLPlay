:- initialization(main).

:- use_module(inicial).
:- use_module(auth).

main :-
    pagina_inicial,
    loop_autenticacao.
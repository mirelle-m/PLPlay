:- consult(inicial).
:- consult(auth).
:- consult(utils).
:- consult(navegacao).

:- consult('dados/stubs').
:- consult('dados/missoes').

:- consult(gerenciador_progresso).
:- consult(jogo).
:- consult(menu).

main :-
    inicial:pagina_inicial,
    auth:loop_autenticacao.
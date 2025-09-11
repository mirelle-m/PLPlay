:- module(navegacao, [
    escolher_opcao_com_titulo/3,
    escolher_opcao/3
]).

:- use_module(utils).

% ----------------------
% Configuração terminal
% ----------------------
configs_temporarias_terminal(Action) :-
    tty_get_mode(user_input, OldMode),
    tty_raw(user_input),
    call_cleanup(Action, tty_set_mode(user_input, OldMode)).

% ----------------------
% Exibir opção
% ----------------------
exibir_opcao(Index, Texto, Selected, Linha) :-
    ( Index =:= Selected ->
        string_concat("-> ", Texto, Linha)
    ;   string_concat("   ", Texto, Linha)
    ).

% ----------------------
% Escolher com logo
% ----------------------
escolher_opcao_com_titulo(Path, Opcoes, Escolhida) :-
    configs_temporarias_terminal(
        escolher_loop(Path, Opcoes, 0, Escolhida)
    ).

escolher_loop(Path, Opcoes, Sel, Escolhida) :-
    length(Opcoes, N),
    utils:limpar_tela_completa,
    utils:mostrar_logo_centralizada(Path),
    forall(nth0(I, Opcoes, Texto),
           ( exibir_opcao(I, Texto, Sel, Linha),
             writeln(Linha) )),
    get_key(Key),
    ( Key = up    -> Sel1 is (Sel - 1 + N) mod N, escolher_loop(Path, Opcoes, Sel1, Escolhida)
    ; Key = down  -> Sel1 is (Sel + 1) mod N, escolher_loop(Path, Opcoes, Sel1, Escolhida)
    ; Key = enter -> Escolhida = Sel
    ; escolher_loop(Path, Opcoes, Sel, Escolhida)
    ).

% ----------------------
% Escolher com título
% ----------------------
escolher_opcao(Titulo, Opcoes, Escolhida) :-
    configs_temporarias_terminal(
        escolher_loop_titulo(Titulo, Opcoes, 0, Escolhida)
    ).

escolher_loop_titulo(Titulo, Opcoes, Sel, Escolhida) :-
    length(Opcoes, N),
    utils:limpar_tela_completa,
    writeln(Titulo),
    forall(nth0(I, Opcoes, Texto),
           ( exibir_opcao(I, Texto, Sel, Linha),
             writeln(Linha) )),
    get_key(Key),
    ( Key = up    -> Sel1 is (Sel - 1 + N) mod N, escolher_loop_titulo(Titulo, Opcoes, Sel1, Escolhida)
    ; Key = down  -> Sel1 is (Sel + 1) mod N, escolher_loop_titulo(Titulo, Opcoes, Sel1, Escolhida)
    ; Key = enter -> Escolhida = Sel
    ; escolher_loop_titulo(Titulo, Opcoes, Sel, Escolhida)
    ).


get_key(Key) :-
    get_char(C1),
    ( C1 = '\e' ->               % ESC
        get_char(_C2),
        get_char(C3),
        ( C3 = 'A' -> Key = up
        ; C3 = 'B' -> Key = down
        ; Key = other )
    ; ( C1 = '\r' ; C1 = '\n' ) -> Key = enter
    ; Key = other
    ).

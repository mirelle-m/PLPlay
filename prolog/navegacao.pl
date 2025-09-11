:- module(navegacao, [
    escolher_opcao_com_titulo/3,
    escolher_opcao/3
]).

:- use_module(utils).

% ----------------------
% Wrapper que coloca o terminal em modo raw enquanto Action executa.
% with_tty_raw/1 garante restauração do estado do terminal. (SWI-Prolog)
% ----------------------
configs_temporarias_terminal(Action) :-
    with_tty_raw(Action).    % built-in SWI-Prolog; lê sem echo e sem esperar CR. 

% ----------------------
% Exibir uma opção (-> quando selecionada)
% ----------------------
exibir_opcao(Index, Texto, Selected, Linha) :-
    ( Index =:= Selected ->
        string_concat("-> ", Texto, Linha)
    ;   string_concat("   ", Texto, Linha)
    ).

% Imprime lista de opções com índice começando em 0
print_options(Opcoes, Selected) :-
    print_options(Opcoes, 0, Selected).

print_options([], _, _).
print_options([T|Ts], I, Sel) :-
    exibir_opcao(I, T, Sel, Line),
    writeln(Line),
    I1 is I + 1,
    print_options(Ts, I1, Sel).

% ----------------------
% Escolher opção mostrando logo (arquivo)
% ----------------------
escolher_opcao_com_titulo(Path, Opcoes, Escolhida) :-
    configs_temporarias_terminal(escolher_loop(Path, Opcoes, 0, Escolhida)).

escolher_loop(Path, Opcoes, Sel, Escolhida) :-
    length(Opcoes, N),
    utils:limpar_tela_completa,
    utils:mostrar_logo_centralizada(Path),
    print_options(Opcoes, Sel),
    get_key(Key),
    ( Key == up    -> Sel1 is (Sel - 1 + N) mod N, escolher_loop(Path, Opcoes, Sel1, Escolhida)
    ; Key == down  -> Sel1 is (Sel + 1) mod N, escolher_loop(Path, Opcoes, Sel1, Escolhida)
    ; Key == enter -> Escolhida = Sel
    ;               escolher_loop(Path, Opcoes, Sel, Escolhida)
    ).

% ----------------------
% Escolher opção mostrando título (string)
% ----------------------
escolher_opcao(Titulo, Opcoes, Escolhida) :-
    configs_temporarias_terminal(escolher_loop_titulo(Titulo, Opcoes, 0, Escolhida)).

escolher_loop_titulo(Titulo, Opcoes, Sel, Escolhida) :-
    length(Opcoes, N),
    utils:limpar_tela_completa,
    writeln(Titulo),
    print_options(Opcoes, Sel),
    get_key(Key),
    ( Key == up    -> Sel1 is (Sel - 1 + N) mod N, escolher_loop_titulo(Titulo, Opcoes, Sel1, Escolhida)
    ; Key == down  -> Sel1 is (Sel + 1) mod N, escolher_loop_titulo(Titulo, Opcoes, Sel1, Escolhida)
    ; Key == enter -> Escolhida = Sel
    ;               escolher_loop_titulo(Titulo, Opcoes, Sel, Escolhida)
    ).

% ----------------------
% Captura de tecla usando get_single_char/1
% Retorna um dos atoms: up, down, enter, other
% - setas geralmente chegam como ESC '[' <A/B/...> (códigos 27,91,65/66)
% - ENTER: 13 (CR) ou 10 (LF)
% ----------------------
get_key(Key) :-
    get_single_char(C1),            % retorna código do char (inteiro)
    ( C1 =:= 27 ->                  % ESC
        get_single_char(_C2),       % normalmente '[' (91)
        get_single_char(C3),
        ( C3 =:= 65 -> Key = up
        ; C3 =:= 66 -> Key = down
        ; Key = other )
    ; C1 =:= 13 -> Key = enter      % CR
    ; C1 =:= 10 -> Key = enter      % LF
    ; Key = other
    ).

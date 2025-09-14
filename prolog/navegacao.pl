:- module(navegacao, [escolher_opcao/3, escolher_opcao_titulo/4]).
:- use_module(utils).


escolher_opcao_titulo(Path, Opcoes, User, Index) :-
    length(Opcoes, N),
    go(0, N, Path, Opcoes, User, Index).


escolher_opcao(Titulo, Opcoes, Index) :-
    length(Opcoes, N),
    go_titulo(0, N, Titulo, Opcoes, Index).

go(Sel, N, Path, Opcoes, User, Index) :-
    utils:limpar_tela_completa,
    ( exists_file(Path) -> utils:mostrar_banner(Path) ; writeln(Path) ),
    format("~n👤 Usuário: ~w~n~n", [User]),
    exibir_opcoes(Sel, Opcoes, 0),
    get_key(Key),
    ( Key = up    -> Sel1 is (Sel - 1 + N) mod N, go(Sel1, N, Path, Opcoes, User, Index)
    ; Key = down  -> Sel1 is (Sel + 1) mod N,     go(Sel1, N, Path, Opcoes, User, Index)
    ; Key = enter -> Index = Sel
    ; Key = quit  -> Index = quit
    ;               go(Sel, N, Path, Opcoes, User, Index)
    ).

go_titulo(Sel, N, Titulo, Opcoes, Index) :-
    utils:limpar_tela_completa,
    writeln(Titulo),
    nl,
    exibir_opcoes(Sel, Opcoes, 0),
    get_key(Key),
    ( Key = up    -> Sel1 is (Sel - 1 + N) mod N, go_titulo(Sel1, N, Titulo, Opcoes, Index)
    ; Key = down  -> Sel1 is (Sel + 1) mod N,     go_titulo(Sel1, N, Titulo, Opcoes, Index)
    ; Key = enter -> Index = Sel
    ; Key = quit  -> Index = quit
    ;               go_titulo(Sel, N, Titulo, Opcoes, Index)
    ).

% exibir_opcoes/3 : imprime lista com destaque na selecionada
exibir_opcoes(_, [], _).
exibir_opcoes(Sel, [H|T], I) :-
    ( I =:= Sel -> format(" > ~w~n", [H])
    ;             format("   ~w~n", [H])
    ),
    I1 is I + 1,
    exibir_opcoes(Sel, T, I1).

get_key(Key) :-
    get_single_char(Code),
    (   Code = 27 ->  % Código para ESC, início de uma sequência de seta
        get_single_char(91), % Caractere '['
        get_single_char(ArrowCode),
        (   ArrowCode = 65 -> Key = up
        ;   ArrowCode = 66 -> Key = down
        ;   ArrowCode = 67 -> Key = right % Mapeado mas não usado nos seus menus
        ;   ArrowCode = 68 -> Key = left  % Mapeado mas não usado
        ;   Key = other % Outra sequência de escape
        )
    ;   Code = 13 -> Key = enter % Código para Enter (CR)
    ;   Code = 10 -> Key = enter % Código para Enter (LF)
    ;   Code = 3  -> Key = quit  % Ctrl+C
    ;   Key = other
    ).
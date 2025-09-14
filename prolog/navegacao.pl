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
    (exists_file(Path) -> utils:mostrar_banner(Path);
        writeln(Path)
    ),
    format("~n👤 Usuário: ~w~n~n", [User]),
    exibir_opcoes(Sel, Opcoes, 0),
    get_key(Key),
    (Key = up -> Sel1 is (Sel - 1 + N) mod N,
        go(Sel1, N, Path, Opcoes, User, Index);
        Key = down  -> Sel1 is (Sel + 1) mod N,
        go(Sel1, N, Path, Opcoes, User, Index);
        Key = enter -> Index = Sel;
        Key = quit  -> Index = quit;
        go(Sel, N, Path, Opcoes, User, Index)
    ).

go_titulo(Sel, N, Titulo, Opcoes, Index) :-
    utils:limpar_tela_completa,
    writeln(Titulo), nl,
    exibir_opcoes(Sel, Opcoes, 0),
    get_key(Key),
    (Key = up    -> Sel1 is (Sel - 1 + N) mod N, go_titulo(Sel1, N, Titulo, Opcoes, Index);
        Key = down  -> 
            Sel1 is (Sel + 1) mod N,     
            go_titulo(Sel1, N, Titulo, Opcoes, Index);
        Key = enter -> Index = Sel;
        Key = quit  -> Index = quit;
        go_titulo(Sel, N, Titulo, Opcoes, Index)
    ).

exibir_opcoes(_, [], _).
exibir_opcoes(Sel, [H|T], I) :-
    (I =:= Sel -> format(" > ~w~n", [H]);
        format("   ~w~n", [H])
    ),
    I1 is I + 1,
    exibir_opcoes(Sel, T, I1).

get_key(Key) :-
    get_single_char(Code),
    (Code = 27 -> 
        get_single_char(91),
        get_single_char(ArrowCode),
        (ArrowCode = 65 -> Key = up;
            ArrowCode = 66 -> Key = down;
            ArrowCode = 67 -> Key = right;
            ArrowCode = 68 -> Key = left;
            Key = other
        );
        Code = 13 -> Key = enter;
        Code = 10 -> Key = enter;
        Code = 3  -> Key = quit;
        Key = other
    ).

escolher_opcao_treino(Titulo, Opcoes, Index) :-
    length(Opcoes, N),
    nl, writeln(Titulo), nl,
    apenas_opcoes(0, N, Opcoes, Index).

apenas_opcoes(Sel, N, Opcoes, Index) :-
    exibir_opcoes(Sel, Opcoes, 0),
    get_key(Key),
    length(Opcoes, NumOpcoes),
    format('\e[~wA', [NumOpcoes]),
    (   Key = up ->
        Sel1 is (Sel - 1 + N) mod N,
        apenas_opcoes(Sel1, N, Opcoes, Index);
    Key = down ->
        Sel1 is (Sel + 1) mod N,
        apenas_opcoes(Sel1, N, Opcoes, Index);
    Key = enter -> Index = Sel;
    Key = quit  -> Index = quit;
        apenas_opcoes(Sel, N, Opcoes, Index)
    ).
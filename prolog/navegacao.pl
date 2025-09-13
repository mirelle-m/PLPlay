:- module(navegacao, [escolher_opcao/3, escolher_opcao_titulo/4]).

:- use_module(library(readutil)).  % para limpar entrada se precisar

% limpar_tela/0 : limpa o terminal (ANSI)
limpar_tela :-
    write('\e[2J\e[H'), flush_output.

mostrar_logo(Path) :-
    exists_file(Path),
    setup_call_cleanup(
        open(Path, read, Stream),
        ler_linhas(Stream),
        close(Stream)
    ).

ler_linhas(Stream) :-
    read_line_to_string(Stream, Line),
    ( Line == end_of_file -> true
    ; writeln(Line),
      ler_linhas(Stream)
    ).
% =============================
% Escolher opção com título fixo
% =============================

escolher_opcao_titulo(Path, Opcoes, User, Index) :-
    length(Opcoes, N),
    go(0, N, Path, Opcoes,User, Index).

% =============================
% Escolher opção com título custom
% =============================

escolher_opcao(Titulo, Opcoes, Index) :-
    length(Opcoes, N),
    go_titulo(0, N, Titulo, Opcoes, Index).

% ------------------------------
% Loop com logo
% ------------------------------

go(Sel, N, Path, Opcoes, User, Index) :-
    limpar_tela,
    mostrar_logo(Path),
    format("👤 Usuário: ~w~n~n", [User]),
    exibir_opcoes(Sel, Opcoes, 0),
    get_key(Key),
    ( Key = up    -> Sel1 is (Sel - 1 + N) mod N, go(Sel1, N, Path, Opcoes,User, Index)
    ; Key = down  -> Sel1 is (Sel + 1) mod N,     go(Sel1, N, Path, Opcoes, User, Index)
    ; Key = enter -> Index = Sel
    ;               go(Sel, N, Path, Opcoes, User, Index)
    ).

% ------------------------------
% Loop com título string
% ------------------------------

go_titulo(Sel, N, Titulo, Opcoes, Index) :-
    limpar_tela,
    writeln(Titulo),
    nl,
    exibir_opcoes(Sel, Opcoes, 0),
    get_key(Key),
    ( Key = up    -> Sel1 is (Sel - 1 + N) mod N, go_titulo(Sel1, N, Titulo, Opcoes, Index)
    ; Key = down  -> Sel1 is (Sel + 1) mod N,     go_titulo(Sel1, N, Titulo, Opcoes, Index)
    ; Key = enter -> Index = Sel
    ;               go_titulo(Sel, N, Titulo, Opcoes, Index)
    ).

% exibir_opcoes/3 : imprime lista com destaque na selecionada
exibir_opcoes(_, [], _).
exibir_opcoes(Sel, [H|T], I) :-
    ( I =:= Sel -> format("-> ~w~n", [H])
    ;             format("   ~w~n", [H])
    ),
    I1 is I + 1,
    exibir_opcoes(Sel, T, I1).

% ------------------------------
% Captura tecla simples
% ------------------------------
% Usamos get_single_char/1 que lê sem ENTER.
% Traduzimos ESC [ A/B para up/down.
% ENTER = código 10.
% ------------------------------

get_key(Key) :-
    get_single_char(C1),
    ( C1 = 27 ->  % ESC
        get_single_char(91),   % [
        get_single_char(C3),
        ( C3 = 65 -> Key = up      % 'A'
        ; C3 = 66 -> Key = down    % 'B'
        ; Key = other
        )
    ; C1 = 10 ; C1 = 13 -> Key = enter   % ENTER (LF ou CR)
    ; Key = other
    ).

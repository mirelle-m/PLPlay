:- module(utils, [
    mostrar_banner/1,
    limpar_tela/0,
    limpar_tela_completa/0,
    centralizar/3,
    centralizar_bloco/3,
    mostrar_logo_centralizada/1,
    pressionar_enter/0
]).

:- use_module(library(readutil)).
:- use_module(library(lists)).

mostrar_banner(Caminho) :-
    ( exists_file(Caminho) ->
        read_file_to_string(Caminho, Conteudo, []),
        writeln(Conteudo)
    ; writeln("⚠️ Banner não encontrado!")
    ).

limpar_tela :-
    format("\e[2J\e[H", []).

limpar_tela_completa :-
    format("\e[3J\e[2J\e[H", []).

terminal_largura(Width) :-
    ( getenv('COLUMNS', S) -> catch(number_string(Width, S), _, Width = 80)
    ; Width = 80
    ).

centralizar(Largura, Texto, Centralizado) :-
    string_length(Texto, Len),
    Espacos is max(0, (Largura - Len) // 2),
    format(string(Centralizado), "~*c~s", [Espacos, 32, Texto]).

centralizar_bloco(Largura, Linhas, Centralizadas) :-
    maplist(centralizar(Largura), Linhas, Centralizadas).

mostrar_logo_centralizada(Caminho) :-
    read_file_to_string(Caminho, Conteudo, []),
    split_string(Conteudo, "\n", "", Linhas),
    terminal_largura(Largura),
    centralizar_bloco(Largura, Linhas, LinhasCentralizadas),
    forall(member(L, LinhasCentralizadas), writeln(L)).

pressionar_enter :-
    writeln("\nPressione Enter para continuar..."),
    read_line_to_string(user_input, _).
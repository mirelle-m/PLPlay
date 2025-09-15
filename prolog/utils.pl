:- module(utils, [
    mostrar_banner/1,
    limpar_tela/0,
    limpar_tela_completa/0,
    centralizar/3,
    centralizar_bloco/3,
    mostrar_logo_centralizada/1,
    mostrar_logo_animada/1,
    pressionar_enter/0,
    terminal_largura/1,
    linha_sep/2,
    pagina_inicial/0
]).

:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(lists)).

mostrar_banner(Caminho) :-
    (exists_file(Caminho) ->
        read_file_to_string(Caminho, Conteudo, []),
        split_string(Conteudo, "\n", "\r\n", Linhas),
        (current_output(Stream),
            stream_property(Stream, tty(true)) ->  terminal_largura(Largura),
            centralizar_bloco(Largura, Linhas, LinhasCentralizadas),
            forall(member(L, LinhasCentralizadas), writeln(L));
            writeln(Conteudo)
        );
        writeln("⚠️ Banner não encontrado!")
    ).

limpar_tela :-
    format("\e[2J\e[H", []).

limpar_tela_completa :-
    format("\e[3J\e[2J\e[H", []).

terminal_largura(Width) :-
    (current_prolog_flag(tty_control, true),
        tty_size(_, Width) -> true;
        Width = 80
    ).

centralizar(Largura, Texto, Centralizado) :-
    string_length(Texto, Len),
    Espacos is max(0, (Largura - Len) // 2),
    format(string(Centralizado), "~*c~s", [Espacos, 32, Texto]).

centralizar_bloco(Largura, Linhas, Centralizadas) :-
    maplist(string_length, Linhas, Comprimentos),
        max_list(Comprimentos, LMax),
        maplist(preencher_direita(LMax), Linhas, LinhasPaddadas),
        Espacos is max(0, (Largura - LMax) // 2),
        maplist(prefix_spaces(Espacos), LinhasPaddadas, Centralizadas).

preencher_direita(LMax, Linha, LinhaPaddada) :-
    string_length(Linha, Len),
    Dif is max(0, LMax - Len),
    length(Cs, Dif),
    maplist(=(' '), Cs),
    string_chars(Espaços, Cs),
    string_concat(Linha, Espaços, LinhaPaddada).

prefix_spaces(N, Linha, Resultado) :-
    string_chars(Linha, Cs),
    length(Espacos, N),
    maplist(=(' '), Espacos),
    append(Espacos, Cs, Cs2),
    string_chars(Resultado, Cs2).

mostrar_logo_centralizada(Caminho) :-
    read_file_to_string(Caminho, Conteudo, []),
    split_string(Conteudo, "\n", "\r\n", Linhas),
    (current_output(Stream),
        stream_property(Stream, tty(true)) ->  terminal_largura(Largura),
            centralizar_bloco(Largura, Linhas, LinhasCentralizadas),
            forall(member(L, LinhasCentralizadas), writeln(L));
        writeln(Conteudo)
    ).

mostrar_logo_animada(Caminho) :-
    read_file_to_string(Caminho, Conteudo, []),
    split_string(Conteudo, "\n", "\r\n", Linhas),
    maplist(string_codes, Linhas, LinhasCodes),
    mostrar_linhas(LinhasCodes).

pressionar_enter :-
    nl, writeln("Pressione Enter para continuar..."),
    read_line_to_string(user_input, _).

pressionar_enter_voltar :-
    nl, writeln("Pressione Enter para voltar..."),
    read_line_to_string(user_input, _).

linha_sep(Largura, Linha) :-
    length(Cs, Largura),
    maplist(=('='), Cs),
    string_chars(Linha, Cs).

pagina_inicial :-
    mostrar_logo_animada("../banners/plplay.txt"),
    sleep(1), nl,
    read_line_to_string(user_input, _),
    limpar_tela.

mostrar_linhas([]).
mostrar_linhas([L|Ls]) :-
    terminal_largura(Largura),
    string_codes(Texto, L),
    centralizar(Largura, Texto, Centralizado),
    writeln(Centralizado),
    sleep(0.1),
    mostrar_linhas(Ls).
:- module(utils, [
    limpar_tela/0,
    limpar_tela_completa/0,
    centralizar/3,
    preencher_direita/3,
    carregar_logo/2,
    centralizar_bloco/3,
    mostrar_logo_centralizada/1,
    remove_aspas/2,
    adiciona_aspas/2
]).

:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(filesex)).

% ---------------------------------------------------
% Limpar tela
% ---------------------------------------------------

limpar_tela :-
    write('\e[2J\e[H'),
    flush_output.

limpar_tela_completa :-
    write('\e[3J\e[2J\e[H'),
    flush_output.

% ---------------------------------------------------
% Centralizar texto
% centralizar(+Largura, +Texto, -TextoCentralizado)
% ---------------------------------------------------

centralizar(Largura, Texto, TextoCentralizado) :-
    string_length(Texto, Len),
    Espacos is max(0, (Largura - Len) // 2),
    make_spaces(Espacos, S),
    string_concat(S, Texto, TextoCentralizado).

make_spaces(N, S) :-
    length(L, N),
    maplist(=(' '), L),
    string_chars(S, L).

% ---------------------------------------------------
% Preencher à direita
% preencher_direita(+Largura, +Texto, -TextoPreenchido)
% ---------------------------------------------------

preencher_direita(N, Texto, TextoFinal) :-
    string_length(Texto, Len),
    Faltam is max(0, N - Len),
    make_spaces(Faltam, S),
    string_concat(Texto, S, TextoFinal).

% ---------------------------------------------------
% Carregar arquivo (logo ASCII)
% carregar_logo(+Caminho, -Linhas)
% ---------------------------------------------------

carregar_logo(Caminho, Linhas) :-
    read_file_to_string(Caminho, Conteudo, []),
    split_string(Conteudo, "\n", "", Linhas).

% ---------------------------------------------------
% Centralizar bloco
% centralizar_bloco(+Largura, +Linhas, -LinhasCentralizadas)
% ---------------------------------------------------

centralizar_bloco(Largura, Linhas, LinhasCentralizadas) :-
    maplist(string_length, Linhas, Comprimentos),
    max_list(Comprimentos, MaxLen),
    maplist(preencher_direita(MaxLen), Linhas, Padded),
    Desloc is max(0, (Largura - MaxLen) // 2),
    make_spaces(Desloc, Espacos),
    maplist({Espacos}/[L,LC]>>string_concat(Espacos, L, LC), Padded, LinhasCentralizadas).

% ---------------------------------------------------
% Mostrar logo centralizada
% mostrar_logo_centralizada(+Caminho)
% ---------------------------------------------------

mostrar_logo_centralizada(Caminho) :-
    carregar_logo(Caminho, Linhas),
    centralizar_bloco(80, Linhas, Centralizadas), % largura fixa 80 cols (pode adaptar)
    maplist(writeln, Centralizadas).

% ---------------------------------------------------
% Manipular aspas
% ---------------------------------------------------

remove_aspas(Str, Result) :-
    string_length(Str, Len),
    ( Len >= 2,
      sub_string(Str, 0, 1, _, "\""),
      sub_string(Str, _, 1, 0, "\"")
    -> sub_string(Str, 1, Len2, 1, Result),
       Len2 is Len - 2
    ;  Result = Str).

adiciona_aspas(Str, Result) :-
    string_concat("\"", Str, Tmp),
    string_concat(Tmp, "\"", Result).

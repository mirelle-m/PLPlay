:- module(inicial, [
    mostrar_logo_animada/1,
    pagina_inicial/0
]).

:- use_module(library(readutil)).   % para ler arquivo
:- use_module(library(apply)).      % para maplist/2 etc.

% --------------------------
% Centralizar texto (simplificado)
% --------------------------
centralizar(Largura, Texto, Centralizado) :-
    string_length(Texto, Len),
    Espacos is max(0, (Largura - Len) // 2),
    format(string(Centralizado), "~*c~s", [Espacos, 32, Texto]).

% --------------------------
% Mostrar logo com animação
% --------------------------
mostrar_logo_animada(Caminho) :-
    read_file_to_string(Caminho, Conteudo, []),
    split_string(Conteudo, "\n", "", Linhas),
    maplist(string_codes, Linhas, LinhasCodes),
    mostrar_linhas(LinhasCodes).

mostrar_linhas([]).
mostrar_linhas([L|Ls]) :-
    terminal_largura(Largura),
    string_codes(Texto, L),
    centralizar(Largura, Texto, Centralizado),
    writeln(Centralizado),
    sleep(0.1),              % 100ms
    mostrar_linhas(Ls).

% --------------------------
% Página inicial
% --------------------------
pagina_inicial :-
    mostrar_logo_animada("../banners/plplay.txt"),
    sleep(1),                  % espera 1 segundo
    writeln("Pressione ENTER para continuar..."),
    read_line_to_string(user_input, _),
    limpar_tela.

% --------------------------
% Funções auxiliares
% --------------------------

% Descobre largura do terminal (simples: assume 80 se não souber)
terminal_largura(80).

limpar_tela :-
    % ANSI escape sequence para limpar tela
    put_code(27), write("[2J"),
    put_code(27), write("[H").
:- module(utils, [
    mostrar_banner/1,
    limpar_tela/0,
    limpar_tela_completa/0,
    centralizar/3,
    centralizar_bloco/3,
    mostrar_logo_centralizada/1,
    remove_aspas/2,
    adiciona_aspas/2,
    pressionar_enter/0,
    get_single_char/1
]).

mostrar_banner(Caminho) :-
    ( exists_file(Caminho) ->
        read_file_to_string(Caminho, Conteudo, []),
        split_string(Conteudo, "\n", "", Linhas),
        forall(member(Linha, Linhas), writeln(Linha))
    ; writeln("⚠️ Banner não encontrado!")
    ).
:- use_module(library(readutil)).
:- use_module(library(lists)).

limpar_tela :-
    format("\e[2J\e[H", []).

limpar_tela_completa :-
    format("\e[3J\e[2J\e[H", []).

centralizar(Largura, Texto, Centralizado) :-
    string_length(Texto, Len),
    Espacos is max(0, (Largura - Len) // 2),
    format(string(Centralizado), "~*c~s", [Espacos, 32, Texto]). % 32 = espaço

centralizar_bloco(Largura, Linhas, Centralizadas) :-
    maplist(string_length, Linhas, Comprimentos),
    max_list(Comprimentos, LMax),
    maplist(preencher_direita(LMax), Linhas, LinhasPaddadas),
    Espacos is max(0, (Largura - LMax) // 2),
    maplist(prefix_spaces(Espacos), LinhasPaddadas, Centralizadas).

preencher_direita(LMax, Linha, LinhaPaddada) :-
    string_length(Linha, Len),
    Dif is LMax - Len,
    string_concat(Linha, Espaços, LinhaPaddada),
    string_chars(Espaços, Cs),
    length(Cs, Dif),
    maplist(=(' '), Cs).

prefix_spaces(N, Linha, Resultado) :-
    string_chars(Linha, Cs),
    length(Espacos, N),
    maplist(=(' '), Espacos),
    append(Espacos, Cs, Cs2),
    string_chars(Resultado, Cs2).

mostrar_logo_centralizada(Caminho) :-
    read_file_to_string(Caminho, Conteudo, []),
    split_string(Conteudo, "\n", "", Linhas),
    current_output(Stream),
    stream_property(Stream, tty(true)), % tenta detectar terminal
    !,
    terminal_largura(Largura),
    centralizar_bloco(Largura, Linhas, LinhasCentralizadas),
    forall(member(L, LinhasCentralizadas), writeln(L)).
mostrar_logo_centralizada(Caminho) :-
    read_file_to_string(Caminho, Conteudo, []),
    split_string(Conteudo, "\n", "", Linhas),
    forall(member(L, Linhas), writeln(L)).

remove_aspas(Str, SemAspas) :-
    string_chars(Str, ['"'|Rest]),
    append(Chars, ['"'], Rest),
    string_chars(SemAspas, Chars), !.
remove_aspas(Str, Str).

adiciona_aspas(Str, ComAspas) :-
    string_concat("\"", Str, Temp),
    string_concat(Temp, "\"", ComAspas).

terminal_largura(Width) :-
    (current_prolog_flag(tty_control, true) ->
        getenv("COLUMNS", S), number_string(Width, S)
    ; Width = 80).  % valor padrão

get_single_char(Char) :-
    shell('stty raw -echo', _),
    get_code(Code),
    shell('stty -raw echo', _),
    (   Code == 27 -> % Sequência de Escape (setas)
        get_code(91),
        get_code(ArrowCode),
        (   ArrowCode == 65 -> Char = 'A' ; % Cima
            ArrowCode == 66 -> Char = 'B' ; % Baixo
            ArrowCode == 67 -> Char = 'C' ; % Direita
            ArrowCode == 68 -> Char = 'D'   % Esquerda
        )
    ;   Code == 13 -> % Tecla Enter
        Char = '\r'
    ;
        char_code(Char, Code) % Outras teclas
    ).

pressionar_enter :-
    writeln("\nPressione Enter para continuar..."),
    read_line_to_string(user_input, _).

testar_teclado :-
    writeln('--- Teste de Teclado ---'),
    writeln('Pressione as setas ou qualquer tecla. Pressione "q" para sair.'),
    loop_teste_teclado.

loop_teste_teclado :-
    get_code(Code),
    format('Código recebido: ~w\n', [Code]),
    (   char_code('q', Code) ->
        writeln('--- Fim do Teste ---')
    ;
        loop_teste_teclado
    ).
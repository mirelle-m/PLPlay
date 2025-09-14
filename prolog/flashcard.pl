:- module(flashcard, [iniciar_treino/0]).

:- use_module(library(readutil)).
:- use_module(library(random)).
:- use_module(utils, [
    limpar_tela/0,
    terminal_largura/1,
    mostrar_banner/1,
    centralizar/3,
    linha_sep/2
]).
:- use_module('../data/flashcards').

carregar_flashcards(Flashcards) :-
    findall(flashcard(P,R), flashcards:flashcard(P,R), Flashcards).

escolher_perguntas_aleatorias(N, Lista, Selecionados) :-
    random_permutation(Lista, Embaralhados),
    length(Embaralhados, Tam),
    Min is min(N, Tam),
    length(Selecionados, Min),
    append(Selecionados, _, Embaralhados).

mostrar_flashcards([]) :-
    write("Você chegou ao fim dos flashcards!"), nl,
    menu_fim_treino.

mostrar_flashcards([flashcard(P,R)|Fs]) :-
    limpar_tela,
    terminal_largura(Largura),
    linha_sep(Largura, Linha),
    writeln(Linha),
    centralizar(Largura, P, PerguntaC),
    nl, write(PerguntaC), nl, nl,
    linha_sep(Largura, Linha),
    writeln(Linha),
    write("\nAperte Enter para ver a resposta\n"),
    read_line_to_string(user_input, _),
    linha_sep(Largura, Linha),
    writeln(Linha),
    centralizar(Largura, R, RespostaC),
    nl, write(RespostaC), nl, nl,
    linha_sep(Largura, Linha),
    writeln(Linha),
    write("\nDigite 'sair' para encerrar o treino ou apenas Enter para continuar\n"),
    read_line_to_string(user_input, Opcao),
    ( string_lower(Opcao, "sair") ->
        write("Treino encerrado!"), nl,
        menu_fim_treino;
        mostrar_flashcards(Fs) ).

iniciar_treino :-
    limpar_tela,
    mostrar_banner('../banners/inicio_treino.txt'), nl,
    write("Pressione Enter para começar..."), nl,
    read_line_to_string(user_input, _),
    carregar_flashcards(Todos),
    ( Todos == [] -> 
        write("Nenhum flashcard carregado!"), nl,
        menu:menu_principal; 
        escolher_perguntas_aleatorias(10, Todos, Selecionados),
        mostrar_flashcards(Selecionados) ).

menu_fim_treino :-
    limpar_tela,
    mostrar_banner('../banners/fim_treino.txt'), nl,
    write("1 - Treinar novamente"), nl,
    write("2 - Voltar ao menu"), nl,
    read_line_to_string(user_input, Opcao),
    ( Opcao = "1" -> iniciar_treino; 
        Opcao = "2" -> menu:menu_principal;
     write("Opção inválida."), nl, menu_fim_treino ).

:- module(flashcards, [iniciar_treino/0]).

:- use_module(library(readutil)).
:- use_module(library(random)).

:- consult(flashcards).

carregar_flashcards(Flashcards) :-
    findall(flashcard(P, R), flashcard(P, R), Flashcards).

escolher_perguntas_aleatorias(N, Lista, Selecionados) :-
    random_permutation(Lista, Embaralhados),
    length(Selecionados, N),
    append(Selecionados, _, Embaralhados).

mostrar_flashcards([]).
mostrar_flashcards([flashcard(P, R)|Fs]) :-
    nl, write("===================================="), nl,
    write("Pergunta: "), write(P), nl, nl,
    write("Pressione Enter para ver a resposta..."), nl,
    read_line_to_string(user_input, _),
    write("Resposta: "), write(R), nl,
    write("===================================="), nl, nl,
    write("Digite 'sair' para encerrar ou Enter para continuar: "), nl,
    read_line_to_string(user_input, Opcao),
    ( string_lower(Opcao, "sair") -> write("Treino encerrado!"), nl; 
    mostrar_flashcards(Fs)).

iniciar_treino :-
    carregar_flashcards(Todos),
    (Todos == [] -> write("Nenhum flashcard carregado!"), nl;
    escolher_perguntas_aleatorias(10, Todos, Selecionados),
        mostrar_flashcards(Selecionados),
        menu_fim_treino).

menu_fim_treino :-
    nl, write("=== Fim do treino ==="), nl,
    write("1 - Treinar novamente"), nl,
    write("2 - Voltar ao menu"), nl,
    read_line_to_string(user_input, Opcao),
    (Opcao = "1" -> iniciar_treino; 
    Opcao = "2" -> write("Voltando ao menu..."), nl;
    write("Opção inválida."), nl, menu_fim_treino).

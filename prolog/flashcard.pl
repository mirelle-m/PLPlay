:- module(flashcard, [iniciar_treino/0]).

:- use_module(library(readutil)).
:- use_module(library(random)).
:- use_module(utils).
:- use_module(navegacao).
:- use_module(menu).
:- use_module(flashcards_fatos).

carregar_flashcards(Flashcards) :-
    findall(flashcard(P,R), flashcards:flashcard(P,R), Flashcards).

escolher_perguntas_aleatorias(N, Lista, Selecionados) :-
    random_permutation(Lista, Embaralhados),
    length(Embaralhados, Tam),
    Min is min(N, Tam),
    length(Selecionados, Min),
    append(Selecionados, _, Embaralhados).

mostrar_flashcards([]) :-
    utils:limpar_tela_completa,
    writeln("✔️ Você concluiu todos os flashcards desta rodada!"),
    writeln("Retornando ao menu principal..."),
    sleep(4),
    menu:menu_principal.

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
    menu_pos_card(Fs).

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

menu_pos_card(Fs) :-
    Opcoes = ["➡️ Próximo card", "🛑 Parar e voltar ao menu principal"],
    navegacao:escolher_opcao_treino("", Opcoes, Escolha),
    tratar_escolha_pos_card(Escolha, Fs).

tratar_escolha_pos_card(0, Fs) :-
    mostrar_flashcards(Fs).
tratar_escolha_pos_card(1, _) :-
    menu:menu_principal.
tratar_escolha_pos_card(quit, _) :-
    menu:menu_principal.
tratar_escolha_pos_card(_, Fs) :- 
    mostrar_flashcards(Fs).
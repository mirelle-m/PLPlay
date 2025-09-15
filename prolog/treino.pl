:- module(treino, [iniciar_treino/0]).

:- use_module(library(readutil)).
:- use_module(library(random)).
:- use_module(auth).
:- use_module(utils).
:- use_module(menu).
:- use_module(perguntas).
:- use_module(navegacao).
:- use_module(flashcards).

carregar_flashcards_gerados(Flashcards) :-
    (exists_file('flashcards.pl') ->
        consult('flashcards.pl'),
        findall(flashcard(P,R), flashcard(P,R), Flashcards);
        Flashcards = []
    ).

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
    sleep(2),
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
    utils:limpar_tela,
    utils:mostrar_banner('../banners/inicio_treino.txt'), nl,
    auth:login_corrente(_, _, _, _, Salvos),
    gerar_flashcards_para_usuario(Salvos),
    carregar_flashcards_gerados(TodosOsFlashcards),    
    (TodosOsFlashcards == [] ->
        writeln('Nenhuma questão salva encontrada para gerar flashcards.'),
        writeln('Pressione Enter para voltar ao menu.'),
        read_line_to_string(user_input, _),
        menu:menu_principal;
        writeln('Pressione Enter para começar...'), nl,
        read_line_to_string(user_input, _),
        escolher_perguntas_aleatorias(10, TodosOsFlashcards, Selecionados),
        mostrar_flashcards(Selecionados)
    ).

gerar_flashcards_para_usuario(ListaDeIDs) :-
    (ListaDeIDs == [] ->
        true;
        open('flashcards.pl', write, Stream),
        writeln(Stream, ':- module(flashcards, [flashcard/2]).'),
        writeln(Stream, ''),
        writeln(Stream, ':- dynamic flashcard/2.'),
        writeln(Stream, ''),
        forall(member(ID, ListaDeIDs),
            (perguntas:pergunta_mestra(ID, _, P, R, _), 
                format(Stream, 'flashcard(~q, ~q).~n', [P, R])
            )
        ),
        close(Stream)
    ).

menu_pos_card(Fs) :-
    Opcoes = ["➡️ Próximo card", "🛑 Parar e voltar ao menu principal"],
    navegacao:submenu("", Opcoes, Escolha),
    tratar_escolha_pos_card(Escolha, Fs).

tratar_escolha_pos_card(0, Fs) :-
    mostrar_flashcards(Fs).
tratar_escolha_pos_card(1, _) :-
    menu:menu_principal.
tratar_escolha_pos_card(quit, _) :-
    menu:menu_principal.
tratar_escolha_pos_card(_, Fs) :- 
    mostrar_flashcards(Fs).

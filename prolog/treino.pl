% TREINO.PL (VERSÃO COMPLETA E CORRIGIDA)

:- module(treino, [
    iniciar_treino/0 % Garante que o predicado principal seja exportado
]).

% --- Módulos Necessários ---
:- use_module(library(readutil)).
:- use_module(library(random)).
:- use_module(auth).       % Para buscar os dados do usuário
:- use_module(perguntas).  % Para buscar os detalhes das perguntas
:- use_module(utils).
:- use_module(navegacao).
:- use_module(menu).

% --- PREDICADO PRINCIPAL (CHAMADO PELO MENU) ---
iniciar_treino :-
    utils:limpar_tela,
    utils:mostrar_banner('../banners/inicio_treino.txt'), nl,
    
    % 1. Pega a lista de IDs de questões salvas do usuário.
    auth:login_corrente(_, _, _, _, Salvos),
    
    % 2. GERA o arquivo de flashcards a partir dessa lista.
    gerar_flashcards_para_usuario(Salvos),
    
    % 3. CARREGA os flashcards do arquivo recém-criado.
    carregar_flashcards_gerados(TodosOsFlashcards),
    
    (   TodosOsFlashcards == [] ->
        writeln('Pressione Enter para voltar ao menu.'),
        read_line_to_string(user_input, _)
    ;
        writeln('Pressione Enter para começar...'), nl,
        read_line_to_string(user_input, _),
        escolher_perguntas_aleatorias(10, TodosOsFlashcards, Selecionados),
        mostrar_flashcards(Selecionados)
    ).

% --- PREDICADOS AUXILIARES (ESTAVAM FALTANDO) ---

% Gera o arquivo 'flashcards.pl' a partir de uma lista de IDs.
gerar_flashcards_para_usuario(ListaDeIDs) :-
    (   ListaDeIDs == [] ->
        writeln('Nenhuma questão salva encontrada para gerar flashcards.')
    ;   
        open('flashcards.pl', write, Stream),
        forall(
            member(ID, ListaDeIDs),
            (   perguntas:pergunta_mestra(ID, _, P, R, _),
                format(Stream, 'flashcard(~q, ~q).~n', [P, R])
            )
        ),
        close(Stream),
        length(ListaDeIDs, Len),
        format('~w flashcards prontos para o treino!~n', [Len])
    ).

% Carrega os fatos do arquivo 'flashcards.pl' recém-criado.
carregar_flashcards_gerados(Flashcards) :-
    (   exists_file('flashcards.pl') ->
        consult('flashcards.pl'),
        findall(flashcard(P,R), flashcard(P,R), Flashcards),
        % Limpa os fatos da memória após carregá-los para a lista.
        abolish(flashcard/2)
    ;
        Flashcards = []
    ).

% --- RESTANTE DO CÓDIGO (JÁ ESTAVA CORRETO) ---

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
    sleep(4). % Apenas termina, o controle volta para o menu.

mostrar_flashcards([flashcard(P,R)|Fs]) :-
    utils:limpar_tela,
    utils:terminal_largura(Largura),
    utils:linha_sep(Largura, Linha),
    writeln(Linha),
    utils:centralizar(Largura, P, PerguntaC),
    nl, write(PerguntaC), nl, nl,
    utils:linha_sep(Largura, Linha),
    writeln(Linha),
    write("\nAperte Enter para ver a resposta\n"),
    read_line_to_string(user_input, _),
    utils:linha_sep(Largura, Linha),
    writeln(Linha),
    utils:centralizar(Largura, R, RespostaC),
    nl, write(RespostaC), nl, nl,
    utils:linha_sep(Largura, Linha),
    writeln(Linha),
    menu_pos_card(Fs).

menu_pos_card(Fs) :-
    Opcoes = ["➡️ Próximo card", "🛑 Parar e voltar ao menu principal"],
    navegacao:escolher_opcao_treino("", Opcoes, Escolha),
    tratar_escolha_pos_card(Escolha, Fs).

tratar_escolha_pos_card(0, Fs) :-
    mostrar_flashcards(Fs).
tratar_escolha_pos_card(1, _) :-
    true. % Apenas termina, voltando para o menu principal.
tratar_colha_pos_card(quit, _) :-
    true.
tratar_escolha_pos_card(_, Fs) :- 
    mostrar_flashcards(Fs).
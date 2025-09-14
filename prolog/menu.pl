:- module(menu, [menu_principal/0]).
:- use_module(jogo).
:- use_module(utils).
:- use_module(navegacao).
:- use_module(missoes).
:- use_module(auth).

menu_principal :-
    limpar_tela_completa,
    login_corrente(User, _, _, _), % Agora login_corrente tem 4 argumentos
    Opcoes = [
        "🎮 Jogar",
        "📰 Ver Regras do Jogo",
        "🗺️  Ver Mapa de Missões",
        "🎯 Modo Treino",
        "🚪 Sair"
    ],
    navegacao:escolher_opcao_titulo("../banners/menu_principal.txt", Opcoes, User, Escolha),
    tratar_escolha(Escolha, User).

tratar_escolha(0, User) :-
    loop_selecao_missao(User),
    menu_principal.

tratar_escolha(1, _) :-
    limpar_tela_completa,
    mostrar_banner('../banners/regras.txt'),
    utils:pressionar_enter,
    menu_principal.

tratar_escolha(2, _) :-
    limpar_tela_completa,
    stubs:imprimir_mapa,
    utils:pressionar_enter,
    menu_principal.

tratar_escolha(3, User) :-
    limpar_tela_completa,
    stubs:iniciar_treino(User),
    utils:pressionar_enter,
    menu_principal.

tratar_escolha(4, _) :-
    writeln("Saindo do jogo... Até a próxima! 👋"),
    auth:salva_usuarios,
    halt.

tratar_escolha(quit, _) :-
    menu_principal.

tratar_escolha(_, _) :-
    writeln("Opção inválida."),
    sleep(1),
    menu_principal.

% Refatoração principal: usa o progresso centralizado
loop_selecao_missao(User) :-
    % Obtém o progresso de missões do usuário em memória
    auth:obter_progresso_completo(User, ProgressoMissao),
    findall(ID-Nome, missoes:missao(ID, Nome), TodasMissoes),
    filtrar_missoes_desbloqueadas(User, ProgressoMissao, TodasMissoes, MissoesDesbloqueadas),
    formatar_opcoes_missao(User, MissoesDesbloqueadas, OpcoesFormatadas),
    append(OpcoesFormatadas, ['<< Voltar'], OpcoesComVoltar),
    navegacao:escolher_opcao_titulo('../banners/missoes.txt', OpcoesComVoltar, User, Escolha),
    
    (   Escolha == quit -> ! ; true ),
    
    length(MissoesDesbloqueadas, Len),
    (   Escolha == Len ->
        !
    ;
        nth0(Escolha, MissoesDesbloqueadas, IDEscolhido-_),
        jogo:iniciar_missao(User, IDEscolhido)
    ).

% Adaptação da formatação para a nova lógica
formatar_opcoes_missao(User, [], []).
formatar_opcoes_missao(User, [ID-Nome | RestoMissoes], [OpcaoFormatada | RestoFormatado]) :-
    auth:obter_progresso_completo(User, Progresso),
    (   member(missao(ID, Acertos), Progresso) ->
        findall(IdQuestao, pergunta_mestra(IdQuestao, ID, _, _, _), TodasQuestoes),
        length(TodasQuestoes, TotalQuestoes),
        length(Acertos, AcertosFeitos),
        Faltantes is TotalQuestoes - AcertosFeitos
    ;
        findall(IdQuestao, pergunta_mestra(IdQuestao, ID, _, _, _), TodasQuestoes),
        length(TodasQuestoes, Faltantes)
    ),
    atom_string(Nome, NomeString),
    format(string(OpcaoFormatada), '~w (~w restantes)', [NomeString, Faltantes]),
    formatar_opcoes_missao(User, RestoMissoes, RestoFormatado).

% Adaptação da filtragem para a nova lógica
filtrar_missoes_desbloqueadas(_, _, [], []).
filtrar_missoes_desbloqueadas(User, Progresso, [ID-Nome | RestoTodas], [ID-Nome | RestoDesbloqueadas]) :-
    ID == 1,
    !,
    filtrar_missoes_desbloqueadas(User, Progresso, RestoTodas, RestoDesbloqueadas).
filtrar_missoes_desbloqueadas(User, Progresso, [ID-Nome | RestoTodas], [ID-Nome | RestoDesbloqueadas]) :-
    IDAnterior is ID - 1,
    member(missao(IDAnterior, _), Progresso),
    !,
    filtrar_missoes_desbloqueadas(User, Progresso, RestoTodas, RestoDesbloqueadas).
filtrar_missoes_desbloqueadas(_, _, _, []).
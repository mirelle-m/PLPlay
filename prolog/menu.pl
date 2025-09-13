:- module(menu, [menu_principal/0]).

:- use_module(jogo).
:- use_module(utils).
:- use_module(navegacao).
:- use_module(auth).
:- use_module(stubs).
:- use_module('dados/missoes').
:- use_module(gerenciador_progresso).

menu_principal :-
    limpar_tela_completa,
    login_corrente(User),
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
    halt.

tratar_escolha(quit, _) :-
    menu_principal.

tratar_escolha(_, _) :-
    writeln("Opção inválida."),
    sleep(1),
    menu_principal.

loop_selecao_missao(User) :-
    findall(ID-Nome, missoes:missao(ID, Nome), TodasMissoes),
    filtrar_missoes_desbloqueadas(User, TodasMissoes, MissoesDesbloqueadas),
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

formatar_opcoes_missao(_, [], []).
formatar_opcoes_missao(User, [ID-Nome | RestoMissoes], [OpcaoFormatada | RestoFormatado]) :-
    gerenciador_progresso:contar_perguntas_faltantes(User, ID, Contagem),
    atom_string(Nome, NomeString),
    format(string(OpcaoFormatada), '~w (~w restantes)', [NomeString, Contagem]),
    formatar_opcoes_missao(User, RestoMissoes, RestoFormatado).

filtrar_missoes_desbloqueadas(_, [], []).
filtrar_missoes_desbloqueadas(User, [ID-Nome | RestoTodas], [ID-Nome | RestoDesbloqueadas]) :-
    ID == 1,
    !,
    filtrar_missoes_desbloqueadas(User, RestoTodas, RestoDesbloqueadas).
filtrar_missoes_desbloqueadas(User, [ID-Nome | RestoTodas], [ID-Nome | RestoDesbloqueadas]) :-
    IDAnterior is ID - 1,
    gerenciador_progresso:verificar_missao_completa(User, IDAnterior),
    !,
    filtrar_missoes_desbloqueadas(User, RestoTodas, RestoDesbloqueadas).
filtrar_missoes_desbloqueadas(_, _, []).
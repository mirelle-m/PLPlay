:- module(menu, [menu_principal/0]).
:- use_module(utils).
:- use_module(navegacao).

% ----------------------
% Menu principal
% ----------------------
menu_principal :-
    Opcoes = [
        "🎮 Jogar",
        "📰 Ver Regras do Jogo",
        "🗺️  Ver Mapa de Missões",
        "🎯 Modo Treino",
        "🚪 Sair"
    ],
    navegação:escolher_opcao_com_titulo("../banners/menu_principal.txt", Opcoes, Escolha),
    utils:limpar_tela_completa,
    tratar_escolha(Escolha).

% ----------------------
% Tratamento das opções
% ----------------------
tratar_escolha(0) :-
    writeln("Iniciando novo jogo..."),
    menu_jogo,       % precisa implementar depois em jogo.pl
    menu_principal.

tratar_escolha(1) :-
    utils:limpar_tela_completa,
    utils:mostrar_logo_centralizada("../banners/regras.txt"),
    writeln("\nPressione Enter para voltar ao menu..."),
    get_char(_),
    menu_principal.

tratar_escolha(2) :-
    utils:limpar_tela_completa,
    imprimir_mapa,   % precisa implementar em missoes.pl
    writeln("\nPressione Enter para voltar ao menu..."),
    get_char(_),
    menu_principal.

tratar_escolha(3) :-
    writeln("Modo Treino"),
    iniciar_treino([]),   % precisa implementar em flashcard.pl
    menu_principal.

tratar_escolha(4) :-
    writeln("Saindo do jogo... Até a próxima! 👋").

tratar_escolha(_) :-
    writeln("Opção inválida."),
    menu_principal.
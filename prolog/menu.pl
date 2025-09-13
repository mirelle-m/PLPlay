:- module(menu, [menu_principal/0]).
:- use_module(utils, [
    limpar_tela/0,
    limpar_tela_completa/0,
    mostrar_logo_centralizada/1,
    mostrar_banner/1
]).
:- use_module(navegacao).
:- use_module(auth). 
:- use_module(flashcard).    
:- use_module(missoes). 

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
    tratar_escolha(Escolha).

tratar_escolha(0) :-
    writeln("Iniciando novo jogo..."),
    menu_jogo,       % precisa implementar depois em jogo.pl
    menu_principal.

tratar_escolha(1) :-
    limpar_tela_completa,
    mostrar_banner('../banners/regras.txt'),
    writeln("\nPressione Enter para voltar ao menu..."),
    get_char(_),
    menu_principal.

tratar_escolha(2) :-
    limpar_tela_completa,
    mostrar_banner('../banners/mapa.txt'),   % precisa implementar em missoes.pl
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

test :- menu_principal. 
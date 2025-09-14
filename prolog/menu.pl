:- module(menu, [menu_principal/0]).

:- use_module(utils).
:- use_module(auth).
:- use_module(jogo).
:- use_module(navegacao).
:- use_module(missoes).
:- use_module(flashcard).

menu_principal :-
    limpar_tela_completa,
    login_corrente(User, _, _, _),
    Opcoes = [
        "🎮 Jogar",
        "📰 Ver Regras do Jogo",
        "🗺️ Ver Mapa de Missões",
        "🎯 Modo Treino",
        "🚪 Sair"
    ],
    navegacao:escolher_opcao_titulo("../banners/menu_principal.txt", Opcoes, User, Escolha),
    tratar_escolha(Escolha, User).

tratar_escolha(0, User) :-
    jogo:iniciar_selecao_missao(User),
    menu_principal.

tratar_escolha(1, _) :-
    limpar_tela_completa,
    mostrar_banner('../banners/regras.txt'),
    utils:pressionar_enter_voltar,
    menu_principal.

tratar_escolha(2, _) :-
    limpar_tela_completa,
    mostrar_banner('../banners/mapa.txt'),
    utils:pressionar_enter_voltar,
    menu_principal.

tratar_escolha(3, User) :-
    limpar_tela_completa,
    flashcard:iniciar_treino.

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

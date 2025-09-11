:- module(menu, [menu_principal/0]).

:- use_module(utils).
:- use_module(navegacao).
:- use_module(jogo).
:- use_module('dados/missoes').
:- use_module(auth).

menu_principal :-
    auth:login_corrente(User),
    utils:limpar_tela_completa,
    format('👤 Usuário: ~w~n', [User]),
    Opcoes = [
        "🎮 Jogar",
        "📰 Ver Regras do Jogo",
        "🚪 Sair"
    ],
    navegacao:escolher_opcao_com_titulo("../banners/menu_principal.txt", Opcoes, Escolha),
    tratar_escolha_principal(Escolha).

tratar_escolha_principal(0) :-
    loop_selecao_missao,
    menu_principal.

tratar_escolha_principal(1) :-
    utils:mostrar_banner('../banners/regras.txt'),
    utils:pressionar_enter,
    menu_principal.

tratar_escolha_principal(2) :-
    writeln("Saindo do jogo... Até a próxima! 👋"),
    halt.

tratar_escolha_principal(quit) :- menu_principal.
tratar_escolha_principal(_) :- menu_principal.

loop_selecao_missao :-
    auth:login_corrente(User),
    findall(Nome, missoes:missao(_, Nome), MissoesNomes),
    append(MissoesNomes, ['<< Voltar'], OpcoesComVoltar),
    navegacao:escolher_opcao_com_titulo('../banners/missoes.txt', OpcoesComVoltar, Escolha),

    (   Escolha == quit ->
        true
    ;
        nth0(Escolha, OpcoesComVoltar, NomeEscolhido),
        (   NomeEscolhido == '<< Voltar' ->
            true
        ;
            missoes:missao(IDEscolhido, NomeEscolhido),
            jogo:iniciar_missao(User, IDEscolhido),
            loop_selecao_missao
        )
    ).
% No topo do arquivo menu.pl

:- module(menu, [
    menu_principal/0  % <-- ESTA LINHA EXPORTA O PREDICADO, CORRIGINDO O ERRO
]).

% --- Módulos Necessários ---
:- use_module(jogo).
:- use_module(treino).
:- use_module(auth).
:- use_module(utils).
:- use_module(navegacao).

% --- PREDICADO PRINCIPAL (AGORA PÚBLICO) ---
menu_principal :-
    utils:limpar_tela_completa,
    % Busca os dados do usuário logado (agora com 5 argumentos)
    auth:login_corrente(User, _, _, _, _),
    Opcoes = [
        "🎮 Jogar",
        "📰 Ver Regras do Jogo",
        "🗺️  Ver Mapa de Missões",
        "🎯 Modo Treino",
        "🚪 Sair"
    ],
    navegacao:escolher_opcao_titulo("../banners/menu_principal.txt", Opcoes, User, Escolha),
    tratar_escolha(Escolha, User).

% --- LÓGICA DE TRATAMENTO DE ESCOLHAS ---
tratar_escolha(0, User) :-
    jogo:iniciar_selecao_missao(User),
    menu_principal.

tratar_escolha(1, _) :-
    utils:limpar_tela_completa,
    utils:mostrar_banner('../banners/regras.txt'),
    utils:pressionar_enter,
    menu_principal.

tratar_escolha(2, _) :-
    utils:limpar_tela_completa,
    utils:mostrar_banner('../banners/mapa.txt'),
    utils:pressionar_enter,
    menu_principal.

tratar_escolha(3, _) :-
    treino:iniciar_treino,
    menu_principal.

tratar_escolha(4, _) :-
    writeln("Finalizando a sessão..."),
    auth:finalizar_sessao,
    writeln("Até a próxima! 👋"),
    halt.

tratar_escolha(quit, _) :-
    menu_principal.

tratar_escolha(_, _) :-
    writeln("Opção inválida."),
    sleep(1),
    menu_principal.
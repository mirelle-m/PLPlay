:- module(menu, [
    menu_principal/0
]).

:- use_module(inicial).      
:- use_module(flashcard).    
:- use_module(jogo).         
:- use_module(missoes).      
:- use_module(utils, [
    limpar_tela/0,
    limpar_tela_completa/0,
    mostrar_logo_centralizada/1,
    mostrar_banner/1
]).

:- use_module(navegacao).    
:- use_module(auth).         

menu_principal :-
    login_corrente(User),                  
    limpar_tela_completa,
    format("👤 Usuário: ~w~n~n", [User]),  
    Opcoes = [
        "🎮 Jogar",
        "📰 Ver Regras do Jogo",
        "🗺️  Ver Mapa de Missões",
        "🎯 Modo Treino",
        "🚪 Sair"
    ],
    escolher_opcao_com_titulo('../banners/menu_principal.txt', Opcoes, Escolha),
    limpar_tela_completa,
    executar_opcao(Escolha).

executar_opcao(0) :- 
    writeln("Iniciando novo jogo..."),
    menu_jogo,
    menu_principal.

executar_opcao(1) :-
    limpar_tela_completa,
    mostrar_banner('../banners/regras.txt'),
    writeln("\nPressione Enter para voltar ao menu..."),
    read_line_to_string(user_input, _),
    menu_principal.

executar_opcao(2) :-
    limpar_tela_completa,
    mostrar_banner('../banners/mapa.txt'),
    writeln("\nPressione Enter para voltar ao menu..."),
    read_line_to_string(user_input, _),
    menu_principal.

executar_opcao(3) :-
    writeln("Modo Treino"),
    iniciar_treino(),
    menu_principal.

executar_opcao(4) :-
    salvar_login_atual,   % Persiste o login atual ao sair
    writeln("Saindo do jogo... Até a próxima! 👋").

executar_opcao(_) :-
    writeln("Opção inválida."),
    menu_principal.

escolher_opcao_com_titulo(BannerPath, Opcoes, Escolha) :-
    mostrar_banner(BannerPath),
    nl,
    enumerate_opcoes(Opcoes, 0),
    writeln("Digite o número da opção desejada:"),
    read_line_to_string(user_input, InputStr),
    ( catch(number_string(Num, InputStr), _, fail), valid_choice(Num, Opcoes) ->
        Escolha = Num
    ; writeln("Entrada inválida, tente novamente."),
      escolher_opcao_com_titulo(BannerPath, Opcoes, Escolha)
    ).

valid_choice(Num, Opcoes) :-
    length(Opcoes, Len),
    Num >= 0,
    Num < Len.

% Enumera opções com índice
enumerate_opcoes([], _).
enumerate_opcoes([O|Os], N) :-
    format("~w - ~w~n", [N, O]),
    N1 is N + 1,
    enumerate_opcoes(Os, N1).

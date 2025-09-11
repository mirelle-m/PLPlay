:- module(navegacao, [
    escolher_opcao/3,
    escolher_opcao_com_titulo/3
]).

:- use_module(utils).

escolher_opcao_com_titulo(Banner, Opcoes, EscolhaIndex) :-
    utils:limpar_tela_completa,
    utils:mostrar_banner(Banner),
    loop_escolha_numerica(Opcoes, EscolhaIndex).

escolher_opcao(Titulo, Opcoes, EscolhaIndex) :-
    utils:limpar_tela_completa,
    writeln(Titulo),
    loop_escolha_numerica(Opcoes, EscolhaIndex).

loop_escolha_numerica(Opcoes, EscolhaFinal) :-
    length(Opcoes, TotalOpcoes),
    desenhar_opcoes_numeradas(Opcoes, 1),
    writeln('\nDigite o número da sua opção (ou 0 para voltar):'),
    read_line_to_string(user_input, Entrada),
    (   atom_number(Entrada, Numero), integer(Numero) ->
        validar_escolha(Numero, TotalOpcoes, Opcoes, EscolhaFinal)
    ;
        writeln('\nEntrada inválida. Por favor, digite apenas um número.'),
        sleep(2),
        utils:limpar_tela_completa,
        loop_escolha_numerica(Opcoes, EscolhaFinal)
    ).

desenhar_opcoes_numeradas([], _).
desenhar_opcoes_numeradas([Opcao|Resto], Index) :-
    format('~w. ~w\n', [Index, Opcao]),
    NovoIndex is Index + 1,
    desenhar_opcoes_numeradas(Resto, NovoIndex).

validar_escolha(0, _, _, quit) :- !.
validar_escolha(Numero, TotalOpcoes, _, EscolhaFinal) :-
    Numero > 0, Numero =< TotalOpcoes,
    EscolhaFinal is Numero - 1.
validar_escolha(_, _, Opcoes, EscolhaFinal) :-
    writeln('\nOpção inválida. Tente novamente.'),
    sleep(2),
    utils:limpar_tela_completa,
    loop_escolha_numerica(Opcoes, EscolhaFinal).
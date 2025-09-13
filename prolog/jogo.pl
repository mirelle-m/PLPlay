:- module(jogo, [
    iniciar_missao/2
]).

:- use_module(navegacao).
:- use_module(gerenciador_progresso).
:- use_module(utils).
:- use_module('dados/stubs').

iniciar_missao(UsuarioID, MissaoID) :-
    carregar_missao(UsuarioID, MissaoID),
    loop_missao(UsuarioID, MissaoID).

loop_missao(UsuarioID, MissaoID) :-
    utils:limpar_tela_completa,
    findall(ID, pergunta_missao(ID, _, _, _, _, nao_acertada), PerguntasDisponiveis),
    take(10, PerguntasDisponiveis, PerguntasDaRodada),

    (   PerguntasDaRodada == [] ->
        writeln('Você já respondeu todas as perguntas desta missão!'),
        sleep(3)
    ;
        length(PerguntasDaRodada, TotalPerguntasRodada),
        realizar_quiz(PerguntasDaRodada, 1, TotalPerguntasRodada, 0, Acertos, 0, Erros),
        finalizar_e_salvar(UsuarioID, MissaoID, Acertos, Erros)
    ).

realizar_quiz([], _, _, Acertos, Acertos, Erros, Erros).
realizar_quiz([PerguntaID|Resto], NumAtual, TotalPerguntas, AccAcertos, TotalAcertos, AccErros, TotalErros) :-
    pergunta_missao(PerguntaID, _, P, RC, Alts, _),
    
    utils:limpar_tela_completa,
    
    format('Questão ~w de ~w:\n\n~w', [NumAtual, TotalPerguntas, P]),
    navegacao:escolher_opcao("", Alts, Escolha),
    
    (   Escolha == quit ->
        TotalAcertos = AccAcertos,
        TotalErros = AccErros
    ;
        nth0(Escolha, Alts, RespostaUsuario),
        (   RespostaUsuario == RC ->
            writeln('\n>> RESPOSTA CORRETA! <<'),
            marcar_acerto(PerguntaID),
            NovoAccAcertos is AccAcertos + 1,
            ProxNum is NumAtual + 1,
            sleep(2),
            realizar_quiz(Resto, ProxNum, TotalPerguntas, NovoAccAcertos, TotalAcertos, AccErros, TotalErros)
        ;
            writeln('\n>> RESPOSTA ERRADA. <<'),
            format('A resposta correta era: ~w\n', [RC]),
            NovoAccErros is AccErros + 1,
            ProxNum is NumAtual + 1,
            sleep(3),
            realizar_quiz(Resto, ProxNum, TotalPerguntas, AccAcertos, TotalAcertos, NovoAccErros, TotalErros)
        )
    ).

finalizar_e_salvar(UsuarioID, MissaoID, Acertos, Erros) :-
    utils:limpar_tela_completa,
    writeln('--- MISSÃO FINALIZADA ---'),
    format('Nesta rodada, você acertou ~w e errou ~w perguntas.\n', [Acertos, Erros]),
    
    (   Acertos >= 7 ->
        writeln('Parabéns, você passou de fase!')
    ;
        true
    ),
    
    writeln('\nSalvando seu progresso...'),
    salvar_missao(UsuarioID, MissaoID),
    utils:pressionar_enter.

take(N, _, []) :- N =< 0.
take(_, [], []).
take(N, [H|T], [H|Resto]) :- N1 is N - 1, take(N1, T, Resto).
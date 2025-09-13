:- module(jogo, [
    iniciar_missao/2
]).
:- use_module(navegacao).
:- use_module(auth).
:- use_module('dados/questoes_fatos').
:- use_module(utils).

% --- PREDICADOS DE INÍCIO E NAVEGAÇÃO ---

iniciar_missao(UsuarioID, MissaoID) :-
    utils:limpar_tela_completa,
    % Obtém o progresso completo do usuário em memória.
    auth:obter_progresso_completo(UsuarioID, ProgressoMissao),
    
    (   member(missao(MissaoID, PerguntasFeitas), ProgressoMissao) ->
        AcertosAnteriores = PerguntasFeitas
    ;
        AcertosAnteriores = []
    ),
    
    % Encontra perguntas que ainda não foram acertadas.
    findall(ID, (pergunta_mestra(ID, MissaoID, _, _, _), \+ member(ID, AcertosAnteriores)), PerguntasDisponiveis),
    
    take(10, PerguntasDisponiveis, PerguntasDaRodada),
    
    (   PerguntasDaRodada == [] ->
        writeln('Você já respondeu todas as perguntas desta missão!'),
        utils:pressionar_enter
    ;
        length(PerguntasDaRodada, TotalPerguntasRodada),
        % `realizar_quiz` agora passa a lista de acertos.
        realizar_quiz(PerguntasDaRodada, UsuarioID, MissaoID, 1, TotalPerguntasRodada, 0, Acertos, 0, Erros, AcertosAnteriores, ListaAcertos),
        
        TotalRespondidas is Acertos + Erros,
        (   TotalRespondidas == 10 ->
            mostrar_resultado_final(UsuarioID, MissaoID, ListaAcertos, Erros)
        ;
            finalizar_e_salvar(UsuarioID, MissaoID, ListaAcertos, Erros)
        )
    ).

% --- PREDICADOS DO QUIZ ---

% `realizar_quiz` agora recebe e retorna a lista de acertos.
realizar_quiz([], _, _, _, AccAcertos, AccAcertos, AccErros, AccErros, AcertosFinais, AcertosFinais).

realizar_quiz([PerguntaID|Resto], User, MissaoID, NumAtual, TotalPerguntas, AccAcertos, TotalAcertos, AccErros, TotalErros, ListaAcertosAtual, AcertosFinais) :-
    
    pergunta_mestra(PerguntaID, MissaoID, P, RC, Alts),
    
    format(string(Placar), 'Acertos: ~w | Erros: ~w', [AccAcertos, AccErros]),
    format(string(TituloQuiz), '~w~n--------------------~nQuestão ~w de ~w:~n~n~w', [Placar, NumAtual, TotalPerguntas, P]),
    
    append(Alts, ['<< Voltar'], OpcoesComVoltar),
    
    navegacao:escolher_opcao(TituloQuiz, OpcoesComVoltar, Escolha),
    
    (   Escolha == quit ->
        TotalAcertos = AccAcertos,
        TotalErros = AccErros,
        AcertosFinais = ListaAcertosAtual
    ;   nth0(Escolha, OpcoesComVoltar, RespostaUsuario),
        (   RespostaUsuario == '<< Voltar' ->
            TotalAcertos = AccAcertos,
            TotalErros = AccErros,
            AcertosFinais = ListaAcertosAtual
        ;   RespostaUsuario == RC ->
            writeln('\n>> RESPOSTA CORRETA! <<'),
            NovoListaAcertos = [PerguntaID|ListaAcertosAtual],
            NovoAccAcertos is AccAcertos + 1,
            ProxNum is NumAtual + 1,
            sleep(1),
            realizar_quiz(Resto, User, MissaoID, ProxNum, TotalPerguntas, NovoAccAcertos, TotalAcertos, AccErros, TotalErros, NovoListaAcertos, AcertosFinais)
        ;   writeln('\n>> RESPOSTA ERRADA. <<'),
            format('A resposta correta era: ~w\n', [RC]),
            NovoAccErros is AccErros + 1,
            ProxNum is NumAtual + 1,
            sleep(3),
            realizar_quiz(Resto, User, MissaoID, ProxNum, TotalPerguntas, AccAcertos, TotalAcertos, NovoAccErros, TotalErros, ListaAcertosAtual, AcertosFinais)
        )
    ).

% --- PREDICADOS DE RESULTADO E SALVAMENTO ---

mostrar_resultado_final(UsuarioID, MissaoID, ListaAcertos, Erros) :-
    utils:limpar_tela_completa,
    
    length(ListaAcertos, Acertos),
    salva_progresso_missao(UsuarioID, MissaoID, Acertos, Erros, ListaAcertos, MissaoAprovada),
    
    (   MissaoAprovada = true ->
        StatusMissao = 'PASSOU'
    ;   StatusMissao = 'NÃO PASSOU'
    ),
    
    Porcentagem is (Acertos * 100) // 10,
    
    writeln('╔══════════════════════════════════════╗'),
    writeln('║          RESULTADO FINAL             ║'),
    writeln('╠══════════════════════════════════════╣'),
    format('║  Questões respondidas: 10            ║~n'),
    format('║  Acertos: ~w                          ║~n', [Acertos]),
    format('║  Erros: ~w                            ║~n', [Erros]),
    format('║  Aproveitamento: ~w%                  ║~n', [Porcentagem]),
    writeln('║                                      ║'),
    
    (   StatusMissao == 'PASSOU' ->
        writeln('║    🎉 PARABÉNS! VOCÊ PASSOU! 🎉     ║')
    ;   writeln('║      😔 VOCÊ NÃO PASSOU 😔          ║'),
        writeln('║        Continue tentando!            ║')
    ),
    
    writeln('╚══════════════════════════════════════╝'),
    writeln(''),
    writeln('Retornando ao menu principal em 3 segundos...'),
    
    sleep(3),
    menu:menu_principal.

salva_progresso_missao(UsuarioID, MissaoID, Acertos, Erros, ListaAcertos, MissaoAprovada) :-
    (   Acertos >= 4 -> % Lógica de aprovação: 4 ou mais acertos
        MissaoAprovada = true
    ;
        MissaoAprovada = false
    ),
    auth:atualiza_progresso_missao(UsuarioID, MissaoID, ListaAcertos, MissaoAprovada).

finalizar_e_salvar(UsuarioID, MissaoID, ListaAcertos, Erros) :-
    utils:limpar_tela_completa,
    writeln('--- TENTATIVA FINALIZADA ---'),
    writeln('Você respondeu a uma rodada de até 10 questões.'),
    length(ListaAcertos, Acertos),
    TotalRespondidas is Acertos + Erros,
    (   TotalRespondidas > 0 ->
        format('Seu desempenho nesta rodada: ~w acertos, ~w erros.~n', [Acertos, Erros])
    ;
        writeln('Você saiu antes de responder alguma questão.')
    ),
    writeln('\nSalvando seu progresso...'),
    salva_progresso_missao(UsuarioID, MissaoID, Acertos, Erros, ListaAcertos, MissaoAprovada),
    (   MissaoAprovada = true ->
        writeln('Status da Missão: VOCÊ PASSOU!')
    ;
        writeln('Status da Missão: CONTINUE TENTANDO!')
    ),
    utils:pressionar_enter.

% --- PREDICADOS AUXILIARES ---

take(N, _, []) :- N =< 0, !.
take(_, [], []) :- !.
take(N, [H|T], [H|Resto]) :- 
    N > 0,
    N1 is N - 1, 
    take(N1, T, Resto).
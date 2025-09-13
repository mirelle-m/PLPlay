:- module(jogo, [
    iniciar_missao/2
]).

:- use_module(navegacao).
:- use_module(gerenciador_progresso).
:- use_module(utils).

iniciar_missao(UsuarioID, MissaoID) :-
    carregar_missao(UsuarioID, MissaoID),
    loop_missao(UsuarioID, MissaoID).

loop_missao(UsuarioID, MissaoID) :-
    utils:limpar_tela_completa,
    findall(ID, pergunta_missao(ID, _, _, _, _, nao_acertada), PerguntasDisponiveis),
    take(10, PerguntasDisponiveis, PerguntasDaRodada),
    (   PerguntasDaRodada == [] ->
        writeln('Você já respondeu todas as perguntas desta missão!'),
        utils:pressionar_enter
    ;
        length(PerguntasDaRodada, TotalPerguntasRodada),
        realizar_quiz(PerguntasDaRodada, UsuarioID, 1, TotalPerguntasRodada, 0, Acertos, 0, Erros),
        % Verifica se completou as 10 questões ou saiu antes
        TotalRespondidas is Acertos + Erros,
        (   TotalRespondidas == 10 ->
            % Respondeu as 10 questões - mostra resultado e volta automaticamente
            mostrar_resultado_final(UsuarioID, MissaoID, Acertos, Erros)
        ;   % Saiu antes das 10 questões - usa o comportamento anterior
            finalizar_e_salvar(UsuarioID, MissaoID, Acertos, Erros)
        )
    ).

realizar_quiz([], _, _, _, AccAcertos, AccAcertos, AccErros, AccErros) :-
    format('DEBUG: Quiz finalizado - Lista vazia. Acertos: ~w, Erros: ~w~n', [AccAcertos, AccErros]).

realizar_quiz([PerguntaID|Resto], User, NumAtual, TotalPerguntas, AccAcertos, TotalAcertos, AccErros, TotalErros) :-
    format('DEBUG: Pergunta ~w de ~w, ID: ~w, Acertos atuais: ~w, Erros atuais: ~w~n', [NumAtual, TotalPerguntas, PerguntaID, AccAcertos, AccErros]),
    
    pergunta_missao(PerguntaID, _, P, RC, Alts, _),
    
    format(string(Placar), 'Acertos: ~w | Erros: ~w', [AccAcertos, AccErros]),
    format(string(TituloQuiz), '~w~n--------------------~nQuestão ~w de ~w:~n~n~w', [Placar, NumAtual, TotalPerguntas, P]),
    
    append(Alts, ['<< Voltar'], OpcoesComVoltar),
    
    navegacao:escolher_opcao(TituloQuiz, OpcoesComVoltar, Escolha),
    format('DEBUG: Escolha do usuário: ~w~n', [Escolha]),
    
    (   Escolha == quit ->
        format('DEBUG: Usuário escolheu quit~n'),
        TotalAcertos = AccAcertos,
        TotalErros = AccErros
    ;   nth0(Escolha, OpcoesComVoltar, RespostaUsuario),
        format('DEBUG: Resposta do usuário: ~w~n', [RespostaUsuario]),
        (   RespostaUsuario == '<< Voltar' ->
            format('DEBUG: Usuário escolheu voltar~n'),
            TotalAcertos = AccAcertos,
            TotalErros = AccErros
        ;   RespostaUsuario == RC ->
            writeln('\n>> RESPOSTA CORRETA! <<'),
            marcar_acerto(PerguntaID),
            NovoAccAcertos is AccAcertos + 1,
            ProxNum is NumAtual + 1,
            format('DEBUG: Resposta correta! Novos acertos: ~w~n', [NovoAccAcertos]),
            sleep(1),
            realizar_quiz(Resto, User, ProxNum, TotalPerguntas, NovoAccAcertos, TotalAcertos, AccErros, TotalErros)
        ;   format('DEBUG: Resposta errada. Correta era: ~w~n', [RC]),
            writeln('\n>> RESPOSTA ERRADA. <<'),
            format('A resposta correta era: ~w\n', [RC]),
            NovoAccErros is AccErros + 1,
            ProxNum is NumAtual + 1,
            format('DEBUG: Resposta errada! Novos erros: ~w~n', [NovoAccErros]),
            sleep(3),
            realizar_quiz(Resto, User, ProxNum, TotalPerguntas, AccAcertos, TotalAcertos, NovoAccErros, TotalErros)
        )
    ).

% Nova função para mostrar resultado final quando completa as 10 questões
mostrar_resultado_final(UsuarioID, MissaoID, Acertos, Erros) :-
    utils:limpar_tela_completa,
    
    % Salva o progresso primeiro
    salvar_missao(UsuarioID, MissaoID),
    
    % Verifica se passou na missão
    (   gerenciador_progresso:verificar_missao_completa(UsuarioID, MissaoID) ->
        StatusMissao = 'PASSOU'
    ;   StatusMissao = 'NÃO PASSOU'
    ),
    
    % Calcula porcentagem de acertos
    Porcentagem is (Acertos * 100) // 10,
    
    % Botar Banner
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
    
    sleep(3).

finalizar_e_salvar(UsuarioID, MissaoID, Acertos, Erros) :-
    utils:limpar_tela_completa,
    writeln('--- TENTATIVA FINALIZADA ---'),
    writeln('Você respondeu a uma rodada de até 10 questões.'),
    TotalRespondidas is Acertos + Erros,
    (   TotalRespondidas > 0 ->
        format('Seu desempenho nesta rodada: ~w acertos, ~w erros.~n', [Acertos, Erros])
    ;
        writeln('Você saiu antes de responder alguma questão.')
    ),
    writeln('\nSalvando seu progresso...'),
    salvar_missao(UsuarioID, MissaoID),
    (   gerenciador_progresso:verificar_missao_completa(UsuarioID, MissaoID) ->
        writeln('Status da Missão: VOCÊ PASSOU!')
    ;
        writeln('Status da Missão: CONTINUE TENTANDO!')
    ),
    utils:pressionar_enter.

take(N, _, []) :- N =< 0, !.
take(_, [], []) :- !.
take(N, [H|T], [H|Resto]) :- 
    N > 0,
    N1 is N - 1, 
    take(N1, T, Resto).
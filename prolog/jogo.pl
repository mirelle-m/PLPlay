:- module(jogo, [iniciar_selecao_missao/1, iniciar_missao/2]).

:- use_module(auth).
:- use_module(missoes).
:- use_module(perguntas).
:- use_module(navegacao).
:- use_module(utils).

iniciar_selecao_missao(User) :-
    writeln('Carregando suas missões...'),
    auth:obter_progresso_nivel(User, ProgressoMissao),
    findall(ID-Nome, missoes:missao(ID, Nome), TodasMissoes),
    filtrar_missoes_desbloqueadas(ProgressoMissao, TodasMissoes, MissoesDesbloqueadas),
    formatar_opcoes_missao(ProgressoMissao, MissoesDesbloqueadas, OpcoesFormatadas),
    append(OpcoesFormatadas, ['<< Voltar'], OpcoesComVoltar),
    navegacao:escolher_opcao_titulo('../banners/missoes.txt', OpcoesComVoltar, User, Escolha),
    processar_escolha_missao(Escolha, MissoesDesbloqueadas, User).

filtrar_missoes_desbloqueadas(_, [], []).
filtrar_missoes_desbloqueadas(Progresso, [ID-Nome | RestoTodas], [ID-Nome | RestoDesbloqueadas]) :-
    ID == 1, !,
    filtrar_missoes_desbloqueadas(Progresso, RestoTodas, RestoDesbloqueadas).

filtrar_missoes_desbloqueadas(Progresso, [ID-Nome | RestoTodas], [ID-Nome | RestoDesbloqueadas]) :-
    IDAnterior is ID - 1,
    member(missao(IDAnterior, _), Progresso), !,
    filtrar_missoes_desbloqueadas(Progresso, RestoTodas, RestoDesbloqueadas).

filtrar_missoes_desbloqueadas(Progresso, [_|RestoTodas], RestoDesbloqueadas) :-
    filtrar_missoes_desbloqueadas(Progresso, RestoTodas, RestoDesbloqueadas).

formatar_opcoes_missao(_, [], []).
formatar_opcoes_missao(Progresso, [ID-Nome | RestoMissoes], [OpcaoFormatada | RestoFormatado]) :-
    findall(IdQuestao, pergunta_mestra(IdQuestao, ID, _, _, _), TodasQuestoes),
    length(TodasQuestoes, TotalQuestoes),
    (member(missao(ID, Acertos), Progresso) ->
        length(Acertos, AcertosFeitos);
        AcertosFeitos = 0
    ),
    Faltantes is TotalQuestoes - AcertosFeitos,
    atom_string(Nome, NomeString),
    format(string(OpcaoFormatada), '~w (~w restantes)', [NomeString, Faltantes]),
    formatar_opcoes_missao(Progresso, RestoMissoes, RestoFormatado).

processar_escolha_missao(quit, _, _) :- !.
processar_escolha_missao(Escolha, Missoes, _) :-
    length(Missoes, Escolha), !.
processar_escolha_missao(Escolha, Missoes, User) :-
    nth0(Escolha, Missoes, IDEscolhido-_),
    iniciar_missao(User, IDEscolhido),
    iniciar_selecao_missao(User). 

iniciar_missao(UsuarioID, MissaoID) :-
    utils:limpar_tela_completa,
    auth:obter_progresso_completo(UsuarioID, ProgressoMissao),
    (member(missao(MissaoID, PerguntasFeitas), ProgressoMissao) -> 
        AcertosAnteriores = PerguntasFeitas;
        AcertosAnteriores = []
    ),
    findall(ID, (pergunta_mestra(ID, MissaoID, _, _, _), \+ member(ID, AcertosAnteriores)), PerguntasDisponiveis),    
    take(10, PerguntasDisponiveis, PerguntasDaRodada),    
    (PerguntasDaRodada == [] ->
        writeln('Você já respondeu todas as perguntas desta missão!'),
        utils:pressionar_enter;
        length(PerguntasDaRodada, TotalPerguntasRodada),
        realizar_quiz(PerguntasDaRodada, UsuarioID, MissaoID, 1, TotalPerguntasRodada, 0, Acertos, 0, Erros, AcertosAnteriores, ListaAcertos),        
        TotalRespondidas is Acertos + Erros,
        (TotalRespondidas == 10 -> 
            mostrar_resultado_final(UsuarioID, MissaoID, ListaAcertos, Erros);
            finalizar_e_salvar(UsuarioID, MissaoID, ListaAcertos, Erros)
        )
    ).

realizar_quiz([], _, _, _, AccAcertos, AccAcertos, AccErros, AccErros, AcertosFinais, AcertosFinais).

realizar_quiz([PerguntaID|Resto], User, MissaoID, NumAtual, TotalPerguntas, AccAcertos, TotalAcertos, AccErros, TotalErros, ListaAcertosAtual, AcertosFinais) :-
    pergunta_mestra(PerguntaID, MissaoID, P, RC, Alts),
    format(string(Placar), 'Acertos: ~w | Erros: ~w', [AccAcertos, AccErros]),
    format(string(TituloQuiz), '~w~n--------------------~nQuestão ~w de ~w:~n~n~w', [Placar, NumAtual, TotalPerguntas, P]),    
    append(Alts, ['<< Voltar'], OpcoesComVoltar),
    navegacao:escolher_opcao(TituloQuiz, OpcoesComVoltar, Escolha),
    (Escolha == quit ->
        TotalAcertos = AccAcertos,
        TotalErros = AccErros,
        AcertosFinais = ListaAcertosAtual;
        nth0(Escolha, OpcoesComVoltar, RespostaUsuario),
        (RespostaUsuario == '<< Voltar' ->
            TotalAcertos = AccAcertos,
            TotalErros = AccErros,
            AcertosFinais = ListaAcertosAtual;
            RespostaUsuario == RC ->
                writeln('\n>> RESPOSTA CORRETA! <<'),
                NovoListaAcertos = [PerguntaID|ListaAcertosAtual],
                NovoAccAcertos is AccAcertos + 1,
                ProxNum is NumAtual + 1,
                sleep(1),
                realizar_quiz(Resto, User, MissaoID, ProxNum, TotalPerguntas, NovoAccAcertos, TotalAcertos, AccErros, TotalErros, NovoListaAcertos, AcertosFinais);
            writeln('\n>> RESPOSTA ERRADA. <<'),
            format('A resposta correta era: ~w\n', [RC]),
            NovoAccErros is AccErros + 1,
            ProxNum is NumAtual + 1,
            sleep(3),
            realizar_quiz(Resto, User, MissaoID, ProxNum, TotalPerguntas, AccAcertos, TotalAcertos, NovoAccErros, TotalErros, ListaAcertosAtual, AcertosFinais)
        )
    ).

mostrar_resultado_final(UsuarioID, MissaoID, ListaAcertos, Erros) :-
    utils:limpar_tela_completa,    
    length(ListaAcertos, Acertos),
    salva_progresso_missao(UsuarioID, MissaoID, Acertos, Erros, ListaAcertos, MissaoAprovada),    
    (MissaoAprovada = true ->
        StatusMissao = 'PASSOU';
        StatusMissao = 'NÃO PASSOU'
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
    (StatusMissao == 'PASSOU' ->
        writeln('║    🎉 PARABÉNS! VOCÊ PASSOU! 🎉     ║');
        writeln('║      😔 VOCÊ NÃO PASSOU 😔          ║'),
        writeln('║        Continue tentando!            ║')
    ),    
    writeln('╚══════════════════════════════════════╝'),
    writeln(''),
    writeln('Retornando ao menu principal em 3 segundos...'),    
    sleep(3),
    menu:menu_principal.

salva_progresso_missao(UsuarioID, MissaoID, Acertos, Erros, ListaAcertos, MissaoAprovada) :-
    (Acertos >= 4 ->
        MissaoAprovada = true;
        MissaoAprovada = false
    ),
    auth:atualiza_progresso_missao(UsuarioID, MissaoID, ListaAcertos, MissaoAprovada).

finalizar_e_salvar(UsuarioID, MissaoID, ListaAcertos, Erros) :-
    utils:limpar_tela_completa,
    writeln('--- TENTATIVA FINALIZADA ---'),
    writeln('Você respondeu a uma rodada de até 10 questões.'),
    length(ListaAcertos, Acertos),
    TotalRespondidas is Acertos + Erros,
    (TotalRespondidas > 0 ->
        format('Seu desempenho nesta rodada: ~w acertos, ~w erros.~n', [Acertos, Erros]);
        writeln('Você saiu antes de responder alguma questão.')
    ),
    writeln('\nSalvando seu progresso...'),
    salva_progresso_missao(UsuarioID, MissaoID, Acertos, Erros, ListaAcertos, MissaoAprovada),
    (MissaoAprovada = true ->
        writeln('Status da Missão: VOCÊ PASSOU!');
        writeln('Status da Missão: CONTINUE TENTANDO!')
    ),
    utils:pressionar_enter.

take(N, _, []) :- N =< 0, !.
take(_, [], []) :- !.
take(N, [H|T], [H|Resto]) :- 
    N > 0,
    N1 is N - 1, 
    take(N1, T, Resto).
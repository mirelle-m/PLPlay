:- module(jogo, [iniciar_selecao_missao/1, iniciar_missao/3]).

:- use_module(utils).
:- use_module(auth).
:- use_module(navegacao).
:- use_module(missoes).
:- use_module(perguntas).
:- use_module(library(lists)).

maximo_erros(facil, 3).
maximo_erros(medio, 2).
maximo_erros(dificil, 1).

iniciar_selecao_missao(User) :-
    writeln('Carregando suas missões...'),
    auth:obter_progresso_nivel(User, NivelStr),
    atom_number(NivelStr, Nivel),
    findall(ID-Nome, missoes:missao(ID, Nome), TodasMissoes),
    include(missao_liberada(Nivel), TodasMissoes, MissoesDesbloqueadas),
    auth:obter_questoes_acertadas(User, QuestoesAcertadas),
    formatar_opcoes_missao_questoes(QuestoesAcertadas, MissoesDesbloqueadas, OpcoesFormatadas),
    append(OpcoesFormatadas, ['🏡 Voltar ao menu principal'], OpcoesComVoltar),
    navegacao:escolher_opcao_titulo('../banners/escolha_estagio.txt', OpcoesComVoltar, User, Escolha),
    processar_escolha_missao(Escolha, MissoesDesbloqueadas, User).

missao_liberada(Nivel, ID-_) :-
    ID =< Nivel.

formatar_opcoes_missao_questoes(_, [], []).
formatar_opcoes_missao_questoes(QuestoesAcertadas, [ID-Nome | Resto], [OpcaoFormatada | RestoFormatado]) :-
    findall(QuestaoID, perguntas:pergunta_mestra(QuestaoID, ID, _, _, _), TodasQuestoes),
    include({QuestoesAcertadas}/[Q]>>member(Q, QuestoesAcertadas), TodasQuestoes, AcertosFeitos),
    length(TodasQuestoes, TotalQuestoes),
    length(AcertosFeitos, AcertosRealizados),
    Faltantes is TotalQuestoes - AcertosRealizados,
    atom_string(Nome, NomeString),
    format(string(OpcaoFormatada), '~w (~w restantes)', [NomeString, Faltantes]),
    formatar_opcoes_missao_questoes(QuestoesAcertadas, Resto, RestoFormatado).

processar_escolha_missao(quit, _, _) :- !.
processar_escolha_missao(Escolha, Missoes, _) :- length(Missoes, Escolha), !.
processar_escolha_missao(Escolha, Missoes, User) :-
    nth0(Escolha, Missoes, IDEscolhido-_),
    OpcoesDificuldade = ['🟢 Fácil (Até 3 erros)', '🟡 Médio (Até 2 erros)', '🔴 Difícil (Até 1 erro)'],
    navegacao:submenu("Escolha a dificuldade da missão:", OpcoesDificuldade, EscolhaDificuldade),
    (EscolhaDificuldade == 0 -> Dificuldade = facil;
        EscolhaDificuldade == 1 -> Dificuldade = medio;
        EscolhaDificuldade == 2 -> Dificuldade = dificil
    ),
    iniciar_missao(User, IDEscolhido, Dificuldade),
    iniciar_selecao_missao(User).

tratar_banner_chefao(MissaoID) :-
    (missoes:banner_chefao(MissaoID, CaminhoDoBanner) ->
        (utils:mostrar_banner(CaminhoDoBanner),
            sleep(1),
            utils:pressionar_enter); true
    ).

iniciar_missao(UsuarioID, MissaoID, Dificuldade) :-
    utils:limpar_tela_completa,
    tratar_banner_chefao(MissaoID),
    auth:obter_questoes_acertadas(UsuarioID, AcertosAnteriores),
    findall(ID, (perguntas:pergunta_mestra(ID, MissaoID, _, _, _),
                 \+ member(ID, AcertosAnteriores)), PerguntasDisponiveis),
    take(10, PerguntasDisponiveis, PerguntasDaRodada),
    ( PerguntasDaRodada == [] ->
        writeln('Você já respondeu todas as perguntas desta missão!'),
        utils:pressionar_enter;
        realizar_quiz(PerguntasDaRodada, UsuarioID, MissaoID, Dificuldade,
                    1, 10, 0, 0, [], ListaAcertos),
      mostrar_resultado_final(UsuarioID, MissaoID, Dificuldade, ListaAcertos)
    ).

realizar_quiz([], _User, _MissaoID, _Dificuldade, _NumAtual, _TotalPerguntas,
             _AccAcertos, _AccErros, ListaAcertos, ListaAcertos).
realizar_quiz([PerguntaID|Resto], User, MissaoID, Dificuldade,
             NumAtual, TotalPerguntas, AccAcertos, AccErros,
             ListaAcertosAtual, AcertosFinais) :-
    maximo_erros(Dificuldade, LimiteErros),
    (AccErros >= LimiteErros ->
        utils:limpar_tela_completa,
        nl, writeln("❌ Você atingiu o limite de erros para essa dificuldade!"),
        sleep(2),
        mostrar_resultado_final(User, MissaoID, Dificuldade, ListaAcertosAtual),
        AcertosFinais = ListaAcertosAtual;
        perguntas:pergunta_mestra(PerguntaID, MissaoID, P, RC, Alts),
        (atom(P) -> PerguntaTexto = P; atom_string(P, PerguntaTexto)),
        format(string(Placar), 'Acertos: ~w | Erros: ~w (Máx: ~w)',
               [AccAcertos, AccErros, LimiteErros]),
        format(string(TituloQuiz),
               '~w~n--------------------~nQuestão ~w de ~w:~n~n~w',
               [Placar, NumAtual, TotalPerguntas, PerguntaTexto]),
        navegacao:escolher_opcao(TituloQuiz, Alts, Escolha),
        (Escolha == quit -> AcertosFinais = ListaAcertosAtual;
            nth0(Escolha, Alts, RespostaUsuario),
            (RespostaUsuario == RC -> nl, writeln('🎉 Parabéns! Você acertou!!'),
                NovoAccAcertos is AccAcertos + 1,
                auth:adicionar_acerto(PerguntaID),
                NovaListaAcertos = [PerguntaID|ListaAcertosAtual],
                mostrar_menu_pos_pergunta(PerguntaID, Resto, User, MissaoID,
                                        Dificuldade, NumAtual, TotalPerguntas,
                                        NovoAccAcertos, AccErros,
                                        NovaListaAcertos, AcertosFinais);
            nl, writeln('❌ Ops... não foi dessa vez! Resposta incorreta!!'),
            format('A resposta correta era: ~w~n', [RC]),
            NovoAccErros is AccErros + 1,
            mostrar_menu_pos_pergunta(PerguntaID, Resto, User, MissaoID,
                                      Dificuldade, NumAtual, TotalPerguntas,
                                      AccAcertos, NovoAccErros,
                                      ListaAcertosAtual, AcertosFinais)
          )
        )
    ).

mostrar_resultado_final(UsuarioID, MissaoID, Dificuldade, ListaAcertos) :-
    utils:limpar_tela_completa,
    
    (   catch(tty_size(_, Width), _, Width = 80) -> true 
    ;   Width = 80
    ),
    utils:mostrar_banner('../banners/resultado_missao.txt'),
    
    length(ListaAcertos, Acertos),
    maximo_erros(Dificuldade, LimiteErros),
    
    % --- Logic to determine status ---
    (   Acertos >= (10 - LimiteErros) ->
        (   auth:obter_progresso_nivel(UsuarioID, NivelAtualStr),
            atom_number(NivelAtualStr, NivelNum),
            (   MissaoID == NivelNum ->
                Status = '˗ˏˋ ★ Próxima missão desbloqueada! ★ ˎˊ˗',
                NovoNivelNum is NivelNum + 1,
                atom_string(NovoNivelNum, NovoNivel),
                auth:adicionar_nivel(NovoNivel)
            ;
                Status = 'Missão concluída com sucesso! (Já desbloqueada anteriormente)'
            )
        )
    ;
        Status = 'Ops... Não foi dessa vez! Vamos tentar de novo?'
    ),
    
    Porcentagem is (Acertos * 100) // 10,
    
    exibir_relatorio(Acertos, Porcentagem, Status, Width),
    
    read_line_to_string(user_input, _),
    menu:menu_principal.

repeat_char(CharAtom, Count, String) :-
    length(List, Count),
    maplist(=(CharAtom), List),
    atomics_to_string(List, String).

print_full_width_line(CharAtom, Width) :-
    repeat_char(CharAtom, Width, LineString),
    writeln(LineString).

gerar_barra_progresso(Porcentagem, Barra) :-
    Preenchidos is Porcentagem // 10,
    Vazios is 10 - Preenchidos,
    length(ListaPreenchidos, Preenchidos),
    maplist(=('▓'), ListaPreenchidos),
    atomics_to_string(ListaPreenchidos, StrPreenchidos),
    length(ListaVazios, Vazios),
    maplist(=('░'), ListaVazios),
    atomics_to_string(ListaVazios, StrVazios),
    format(string(Barra), '[~s~s]', [StrPreenchidos, StrVazios]).

exibir_relatorio(Acertos, Porcentagem, Status, Width) :-
    BlockContentWidth = 46, 
    BlockPadding is max(0, (Width - BlockContentWidth) // 2),

    gerar_barra_progresso(Porcentagem, BarraProgresso),
    nl,
    print_full_width_line('-', Width),

    Header1 = 'RELATÓRIO DA MISSÃO📊',
    atom_length(Header1, Header1Width),
    Header1Padding is max(0, (Width - Header1Width) // 2),
    format('~t~*|~s~n', [Header1Padding, Header1]),

    print_full_width_line('-', Width),
    
    format('~t~*|» Questões respondidas .... 10~n', [BlockPadding]),
    format('~t~*|» Acertos ................. ~w~n', [BlockPadding, Acertos]),
    format('~t~*|» Aproveitamento .......... ~w% ~s~n~n', [BlockPadding, Porcentagem, BarraProgresso]),

    (   sub_string(Status, _, _, _, 'Não foi dessa vez') ->
        (   Header2 = 'STATUS: Missão Falhou! ✖',
            StatusHeader = Header2
        )
    ;
        (   Header3 = 'STATUS: Missão Cumprida!',
            StatusHeader = Header3
        )
    ),
    
    print_full_width_line('-', Width),

    atom_length(StatusHeader, StatusHeaderWidth),
    StatusHeaderPadding is max(0, (Width - StatusHeaderWidth) // 2),
    format('~t~*|~s~n', [StatusHeaderPadding, StatusHeader]),
    
    print_full_width_line('-', Width),

    format('~t~*|~w~n~n', [BlockPadding, Status]),
    format('~t~*|Pressione Enter para retornar ao menu...~n', [BlockPadding]),
    format('~t~*||> ', [BlockPadding]).

take(N, _, []) :- N =< 0, !.
take(_, [], []) :- !.
take(N, [H|T], [H|Resto]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Resto).

mostrar_menu_pos_pergunta(PerguntaID, Resto, User, MissaoID, Dificuldade,
                          NumAtual, TotalPerguntas, 
                          AccAcertos, AccErros, ListaAcertos, AcertosFinais) :-
    Opcoes = ["➡️ Continuar missão", "💾 Salvar como flashcard", "🛑 Voltar ao menu"],
    navegacao:submenu("O que deseja fazer?", Opcoes, Escolha),
    (Escolha == quit -> AcertosFinais = ListaAcertos;
        Escolha == 0 -> ProxNum is NumAtual + 1,
            realizar_quiz(Resto, User, MissaoID, Dificuldade,
                        ProxNum, TotalPerguntas,
                        AccAcertos, AccErros,
                        ListaAcertos, AcertosFinais);
        Escolha == 1 -> auth:adicionar_questao_salva(PerguntaID),
            writeln("✅ Questão salva como flashcard!"),
            ProxNum is NumAtual + 1,
            realizar_quiz(Resto, User, MissaoID, Dificuldade,
                        ProxNum, TotalPerguntas,
                        AccAcertos, AccErros,
                        ListaAcertos, AcertosFinais);
        Escolha == 2 -> menu:menu_principal
    ).
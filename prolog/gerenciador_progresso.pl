:- module(gerenciador_progresso, [
    carregar_missao/2,
    salvar_missao/2,
    marcar_acerto/1,
    pergunta_missao/6
]).

:- use_module('dados/stubs').

:- dynamic pergunta_missao/6.

carregar_missao(UsuarioID, MissaoID) :-
    retractall(pergunta_missao(_,_,_,_,_,_)),
    format(atom(Arquivo), 'progresso/progresso_~w_~w.pl', [UsuarioID, MissaoID]),
    (   exists_file(Arquivo) ->
        consult(Arquivo)
    ;
        inicializar_progresso(MissaoID)
    ).

salvar_missao(UsuarioID, MissaoID) :-
    format(atom(Arquivo), 'progresso/progresso_~w_~w.pl', [UsuarioID, MissaoID]),
    tell(Arquivo),
    listing(pergunta_missao/6),
    told.

inicializar_progresso(MissaoID) :-
    forall(
        stubs:pergunta_mestra(Id, MissaoID, P, RC, Alts),
        assertz(pergunta_missao(Id, 0, P, RC, Alts, nao_acertada))
    ).

marcar_acerto(PerguntaID) :-
    retract(pergunta_missao(PerguntaID, Nivel, P, RC, Alts, nao_acertada)),
    assertz(pergunta_missao(PerguntaID, Nivel, P, RC, Alts, acertada)).
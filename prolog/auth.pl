:- module(auth, [
    loop_autenticacao/0,
    login_corrente/4,
    autenticar_ou_cadastrar/2,
    usuario_corrente/4,
    test/0,

    % ADICIONAR ESTES:
    salva_usuarios/0,
    adicionar_missao_concluida/1,
    adicionar_nivel/1,
    obter_progresso_missoes/2,
    obter_progresso_nivel/2
]).

:- use_module(library(readutil)).
:- use_module(menu).
:- use_module(utils).

:- dynamic usuario_corrente/4.
:- dynamic usuario/4.

carrega_usuarios :-
    retractall(usuario(_,_,_,_)),
    (exists_file('usuarios.pl') -> consult('usuarios.pl');
        true).

salva_usuarios :-
    open('usuarios.pl', write, Stream),
    forall(usuario(U,S,N,M),
           format(Stream, 'usuario(~q, ~q, ~q, ~q).~n', [U,S,N,M])),
    close(Stream).

loop_autenticacao :-
    carrega_usuarios,
    (autenticar_usuario ->  menu:menu_principal;
        writeln("Falha na autenticação."),
        sleep(1),
        loop_autenticacao).

autenticar_usuario :-
    mostrar_banner('../banners/autenticacao.txt'),
    writeln("Digite seu nome de usuário:"),
    read_line_to_string(user_input, Username),
    writeln("Digite sua senha:"),
    read_line_to_string(user_input, Senha),
    autenticar_ou_cadastrar(Username, Senha).

autenticar_ou_cadastrar(Username, Senha) :-
    (usuario(Username, StoredSenha, ProgressoNivel, ProgressoMissao) ->
        (Senha == StoredSenha ->
            writeln("✅ Autenticado com sucesso!"),
            retractall(usuario_corrente(_,_,_,_)),
            asserta(usuario_corrente(Username, Senha, ProgressoNivel, ProgressoMissao));
            writeln("❌ Senha incorreta! Tente novamente!"),
            fail
        );
        writeln("✅ Usuário não encontrado. Cadastro realizado com sucesso!"),
        assertz(usuario(Username, Senha, "1", [])),
        retractall(usuario_corrente(_,_,_,_)),
        asserta(usuario_corrente(Username, Senha, "1", []))
    ).

login_corrente(Username, Senha, ProgressoNivel, ProgressoMissao) :-
    usuario_corrente(Username, Senha, ProgressoNivel, ProgressoMissao).

obter_progresso_missoes(UsuarioID, ProgressoMissao) :-
    usuario_corrente(UsuarioID, _, _, ProgressoMissao).

obter_progresso_nivel(UsuarioID, ProgressoNivel) :-
    usuario_corrente(UsuarioID, _, ProgressoNivel, _).

adicionar_missao_concluida(MissaoID) :-
    usuario_corrente(UsuarioID, Senha, ProgressoNivel, ProgressoAtual),
    (member(MissaoID, ProgressoAtual) ->
        writeln('Aviso: Esta missão já foi registrada como concluída.'),
        NovaListaProgresso = ProgressoAtual;
        NovaListaProgresso = [MissaoID | ProgressoAtual]
    ),
    retract(usuario(UsuarioID, _, _, _)),
    assertz(usuario(UsuarioID, Senha, ProgressoNivel, NovaListaProgresso)),
    retract(usuario_corrente(UsuarioID, _, _, _)),
    asserta(usuario_corrente(UsuarioID, Senha, ProgressoNivel, NovaListaProgresso)).

adicionar_nivel(NovoNivel) :-
    usuario_corrente(UsuarioID, Senha, _, ProgressoAtual),
    retract(usuario(UsuarioID, _, _, _)),
    assertz(usuario(UsuarioID, Senha, NovoNivel, ProgressoAtual)),
    retract(usuario_corrente(UsuarioID, _, _, _)),
    asserta(usuario_corrente(UsuarioID, Senha, NovoNivel, ProgressoAtual)).

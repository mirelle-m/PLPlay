:- module(auth, [
    loop_autenticacao/0,
    login_corrente/4,
    salva_estado_usuario/0,
    atualiza_progresso_missao/4,
    obter_progresso_completo/2,
    autenticar_ou_cadastrar/2,
    usuario_atual/1
]).

:- use_module(library(readutil)).
:- use_module(menu).

:- dynamic usuario/4.
:- dynamic usuario_atual/1.

% ------------------------------
% Persistência
% ------------------------------
carrega_usuarios :-
    retractall(usuario(_, _, _, _)),
    (   exists_file('dados/usuarios_fatos.pl')
    ->  consult('dados/usuarios_fatos.pl')
    ;   true).

salva_usuarios :-
    open('dados/usuarios_fatos.pl', write, Stream),
    forall(usuario(U,S,N,M),
           format(Stream, 'usuario(~q, ~q, ~q, ~q).~n', [U,S,N,M])),
    close(Stream).

salva_estado_usuario :- salva_usuarios.

mostrar_banner(Caminho) :-
    ( exists_file(Caminho) ->
        read_file_to_string(Caminho, Conteudo, []),
        split_string(Conteudo, "\n", "", Linhas),
        forall(member(Linha, Linhas), writeln(Linha))
    ; writeln("⚠️ Banner não encontrado!")
    ).


% ------------------------------
% Autenticação
% ------------------------------
loop_autenticacao :-
    carrega_usuarios,
    (   autenticar_usuario
    ->  menu:menu_principal
    ;   writeln("Falha na autenticação."), sleep(1),
        loop_autenticacao).

autenticar_usuario :-
    mostrar_banner('../banners/autenticacao.txt'),
    writeln("Digite seu nome de usuário:"),
    read_line_to_string(user_input, Username),
    writeln("Digite sua senha:"),
    read_line_to_string(user_input, Senha),
    autenticar_ou_cadastrar(Username, Senha).

autenticar_ou_cadastrar(Username, Senha) :-
    (   usuario(Username, Senha, _, _) ->
        retractall(usuario_atual(_)),
        assertz(usuario_atual(Username)),      
        writeln("✅ Autenticado com sucesso!")
    ;   usuario(Username, _, _, _) ->
        writeln("❌ Senha incorreta! Tente novamente!"), fail
    ;   assertz(usuario(Username, Senha, "1", [])),
        salva_usuarios,
        retractall(usuario_atual(_)),          
        assertz(usuario_atual(Username)),      
        writeln("✅ Cadastro realizado com sucesso!")
    ).

login_corrente(Username, Senha, ProgressoNivel, ProgressoMissao) :-
    usuario(Username, Senha, ProgressoNivel, ProgressoMissao).

% ------------------------------
% Progresso de Missões
% ------------------------------
atualiza_progresso_missao(UsuarioID, MissaoID, ListaAcertos, _Aprovada) :-
    usuario(UsuarioID, Senha, ProgressoNivel, ProgressoMissao),
    (   substituir_missao(ProgressoMissao, MissaoID, ListaAcertos, NovoProgressoMissao)
    ->  true
    ;   append(ProgressoMissao, [missao(MissaoID, ListaAcertos)], NovoProgressoMissao)
    ),
    retract(usuario(UsuarioID, Senha, ProgressoNivel, ProgressoMissao)),
    assertz(usuario(UsuarioID, Senha, ProgressoNivel, NovoProgressoMissao)),
    salva_usuarios.  % ← salva somente quando há mudança

obter_progresso_completo(UsuarioID, ProgressoMissao) :-
    usuario(UsuarioID, _, _, ProgressoMissao).

% ------------------------------
% Auxiliares
% ------------------------------
substituir_missao([], _, _, _) :- fail.
substituir_missao([missao(ID, _)|T], ID, NewAcertos, [missao(ID, NewAcertos)|T]).
substituir_missao([H|T], ID, NewAcertos, [H|T2]) :-
    substituir_missao(T, ID, NewAcertos, T2).

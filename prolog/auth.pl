:- module(auth, [
    loop_autenticacao/0,
    login_corrente/1
]).

:- use_module(library(csv)).
:- use_module(library(readutil)).
:- use_module(utils). % Depende de utils para banners e limpeza de tela

:- dynamic login_atual/1.

loop_autenticacao :-
    repeat,
        (   autenticar_usuario -> !  % Se autenticar, corta o 'repeat' e tem sucesso
        ;   writeln("Falha na autenticação. Tente novamente."),
            sleep(2),
            fail % Força o 'repeat' a tentar de novo
        ).

autenticar_usuario :-
    utils:limpar_tela_completa,
    utils:mostrar_banner('../banners/autenticacao.txt'),
    writeln("Digite seu username:"),
    read_line_to_string(user_input, Username),
    writeln("Digite sua senha:"),
    read_line_to_string(user_input, Senha),
    autenticar_ou_cadastrar(Username, Senha).

autenticar_ou_cadastrar(Username, Senha) :-
    (   exists_file('../data/usuarios.csv') ->
        csv_read_file('../data/usuarios.csv', Rows, [functor(usuario), arity(3)]),
        (   member(usuario(Username, Senha, _), Rows) ->
            writeln("✅ Autenticado com sucesso!"), sleep(1),
            atualiza_login_atual_memoria(Username)
        ;   member(usuario(Username, _, _), Rows) ->
            writeln("❌ Senha incorreta!"), fail
        ;   append(Rows, [usuario(Username, Senha, "1")], Novos),
            csv_write_file('../data/usuarios.csv', Novos, []),
            writeln("✅ Cadastro realizado com sucesso!"), sleep(1),
            atualiza_login_atual_memoria(Username)
        )
    ;   % Arquivo não existe
        csv_write_file('../data/usuarios.csv', [usuario(Username, Senha, "1")], []),
        writeln("✅ Cadastro realizado com sucesso!"), sleep(1),
        atualiza_login_atual_memoria(Username)
    ).

atualiza_login_atual_memoria(Username) :-
    retractall(login_atual(_)),
    assertz(login_atual(Username)).

login_corrente(Username) :-
    login_atual(Username).
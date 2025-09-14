:- module(auth, [
    loop_autenticacao/0,
    login_corrente/1
]).

:- use_module(library(readutil)).
:- use_module(library(csv)).
:- use_module(menu).  

:- dynamic login_atual/1.  % Login atual armazenado em memória

loop_autenticacao :-
    ( autenticar_usuario ->
        menu_principal
    ; writeln("Falha na autenticação."),
      loop_autenticacao
    ).


validar_username(Nome, ok) :-
    string_length(Nome, L),
    L >= 3,
    L =< 15,
    string_codes(Nome, Codes),
    forall(member(C, Codes), (char_type(C, alnum) ; C = 0'_)), !.
validar_username(_, erro("Nome de usuário inválido!")).


validar_senha(Senha, ok) :-
    string_length(Senha, L),
    L >= 6,
    string_codes(Senha, Codes),
    include(char_type_upper, Codes, Uppers), Uppers \= [],
    include(char_type_lower, Codes, Lowers), Lowers \= [],
    include(char_type_digit, Codes, Digits), Digits \= [],
    !.
validar_senha(_, erro("Senha inválida!")).

char_type_upper(C) :- char_type(C, upper).
char_type_lower(C) :- char_type(C, lower).
char_type_digit(C) :- char_type(C, digit).


mostrar_banner(Caminho) :-
    ( exists_file(Caminho) ->
        read_file_to_string(Caminho, Conteudo, []),
        split_string(Conteudo, "\n", "", Linhas),
        forall(member(Linha, Linhas), writeln(Linha))
    ; writeln("⚠️ Banner não encontrado!")
    ).


autenticar_usuario :-
    mostrar_banner('../banners/autenticacao.txt'),
    writeln("Digite seu username:"),
    read_line_to_string(user_input, Username),
    validar_username(Username, ResUser),
    ( ResUser = erro(Msg) ->
        format("Erro: ~s~n", [Msg]), fail
    ; true
    ), nl,
    writeln("Digite sua senha:"),
    read_line_to_string(user_input, Senha),
    validar_senha(Senha, ResSenha),
    ( ResSenha = erro(Msg2) ->
        format("Erro: ~s~n", [Msg2]), fail
    ; true
    ),
    autenticar_ou_cadastrar(Username, Senha).


autenticar_ou_cadastrar(Username, Senha) :-
    ( exists_file('../data/usuarios.csv') ->
        csv_read_file('../data/usuarios.csv', Rows, [functor(usuario), arity(3)]),
        ( member(usuario(Username, Senha, _), Rows) ->
            writeln("✅ Autenticado com sucesso!"),
            atualiza_login_atual_memoria(Username)
        ; member(usuario(Username, OutraSenha, _), Rows), OutraSenha \= Senha ->
            writeln("❌ Senha incorreta! Tente novamente!"), fail
        ; append(Rows, [usuario(Username, Senha, "1")], Novos),
          csv_write_file('../data/usuarios.csv', Novos, []),
          writeln("✅ Cadastro realizado com sucesso!"),
          atualiza_login_atual_memoria(Username)
        )
    ; % Arquivo não existe, cria com o primeiro usuário
      csv_write_file('../data/usuarios.csv', [usuario(Username, Senha, "1")], []),
      writeln("✅ Cadastro realizado com sucesso!"),
      atualiza_login_atual_memoria(Username)
    ).


atualiza_login_atual_memoria(Username) :-
    retractall(login_atual(_)),
    assertz(login_atual(Username)).

% Consulta o login atual em memória
login_corrente(Username) :-
    login_atual(Username).

salvar_login_atual :-
    login_corrente(User),
    open('../data/login_atual.txt', write, Stream),
    write(Stream, User),
    close(Stream).

carregar_login_inicial :-
    ( exists_file('../data/login_atual.txt') ->
        read_file_to_string('../data/login_atual.txt', User, []),
        string_trim(User, TrimUser),
        assertz(login_atual(TrimUser))
    ; true
    ).

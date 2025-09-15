:- module(auth, [
    loop_autenticacao/0,
    autenticar_usuario/0,
    login_corrente/5,
    autenticar_ou_cadastrar/2,
    salva_usuarios/0,
    adicionar_acerto/1,
    adicionar_questao_salva/1,
    adicionar_nivel/1,
    obter_questoes_acertadas/2,
    obter_questoes_salvas/2,
    obter_progresso_nivel/2
]).

:- use_module(library(readutil)).
:- use_module(menu).
:- use_module(utils).

:- dynamic usuario/5.
:- dynamic usuario_corrente/5.


validar_senha(Senha) :-
    atom_length(Senha, Tamanho),
    Tamanho >= 6,  
    atom_chars(Senha, Chars),
    tem_maiuscula(Chars),
    tem_minuscula(Chars),
    tem_digito(Chars),
    tem_especial(Chars).

tem_maiuscula(Chars) :-
    member(C, Chars),
    char_type(C, upper), !.

tem_minuscula(Chars) :-
    member(C, Chars),
    char_type(C, lower), !.

tem_digito(Chars) :-
    member(C, Chars),
    char_type(C, digit), !.

tem_especial(Chars) :-
    member(C, Chars),
    \+ char_type(C, alnum),  
    \+ char_type(C, space), !.

carrega_usuarios :-
    retractall(usuario(_,_,_,_,_)),
    (exists_file('usuarios.pl') -> consult('usuarios.pl'); true).

salva_usuarios :-
    open('usuarios.pl', write, Stream),
    forall(usuario(U,S,N,A,SV),
           format(Stream, 'usuario(~q, ~q, ~q, ~q, ~q).~n', [U,S,N,A,SV])),
    close(Stream).

loop_autenticacao :-
    carrega_usuarios,
    utils:limpar_tela,
    (autenticar_usuario ->  menu:menu_principal;
        sleep(1),
        loop_autenticacao
    ).

autenticar_usuario :-
    mostrar_banner('../banners/autenticacao.txt'),
    writeln("Digite seu nome de usuário:"),
    read_line_to_string(user_input, Username),
    writeln("Digite sua senha:"),
    read_line_to_string(user_input, SenhaStr),
    atom_string(Senha, SenhaStr), 
    autenticar_ou_cadastrar(Username, Senha).

autenticar_ou_cadastrar(Username, Senha) :-
    (usuario(Username, StoredSenha, Nivel, Acertos, Salvos) ->
        (Senha == StoredSenha ->
            nl, writeln("✅ Autenticado com sucesso!"),
            retractall(usuario_corrente(_,_,_,_,_)),
            asserta(usuario_corrente(Username, Senha, Nivel, Acertos, Salvos)),
            writeln("Pressione ENTER para continuar..."),
            read_line_to_string(user_input, _);
            nl, writeln("❌ Senha incorreta! Tente novamente!"), nl,
            sleep(1),
            fail
        );
        (validar_senha(Senha) ->
            nl, writeln("✅ Usuário não encontrado. Cadastro realizado com sucesso!"),
            sleep(1),
            assertz(usuario(Username, Senha, "1", [], [])),
            retractall(usuario_corrente(_,_,_,_,_)),
            asserta(usuario_corrente(Username, Senha, "1", [], [])),
            treino:gerar_flashcards_para_usuario([]);
            nl, writeln("❌ Senha não atende aos critérios de segurança!"),
            writeln("A senha deve ter pelo menos: 6 caracteres, uma letra maiúscula, uma letra minúscula, um dígito e um caracter especial."),
            writeln("Pressione ENTER para tentar novamente..."),
            read_line_to_string(user_input, _),
            fail
        )
    ).

login_corrente(Username, Senha, Nivel, Acertos, Salvos) :-
    usuario_corrente(Username, Senha, Nivel, Acertos, Salvos).

obter_questoes_acertadas(UsuarioID, Acertos) :-
    usuario_corrente(UsuarioID, _, _, Acertos, _).

obter_questoes_salvas(UsuarioID, Salvos) :-
    usuario_corrente(UsuarioID, _, _, _, Salvos).

obter_progresso_nivel(UsuarioID, Nivel) :-
    usuario_corrente(UsuarioID, _, Nivel, _, _).

adicionar_acerto(QuestaoID) :-
    usuario_corrente(UsuarioID, Senha, Nivel, AcertosAtuais, Salvos),
    (member(QuestaoID, AcertosAtuais) -> NovaListaAcertos = AcertosAtuais;
        NovaListaAcertos = [QuestaoID | AcertosAtuais]
    ),
    retract(usuario(UsuarioID, _, _, _, _)),
    assertz(usuario(UsuarioID, Senha, Nivel, NovaListaAcertos, Salvos)),
    retract(usuario_corrente(UsuarioID, _, _, _, _)),
    asserta(usuario_corrente(UsuarioID, Senha, Nivel, NovaListaAcertos, Salvos)).

adicionar_questao_salva(QuestaoID) :-
    usuario_corrente(UsuarioID, Senha, Nivel, Acertos, SalvosAtuais),
    (member(QuestaoID, SalvosAtuais) -> NovaListaSalvos = SalvosAtuais;
        NovaListaSalvos = [QuestaoID | SalvosAtuais]
    ),
    retract(usuario(UsuarioID, _, _, _, _)),
    assertz(usuario(UsuarioID, Senha, Nivel, Acertos, NovaListaSalvos)),
    retract(usuario_corrente(UsuarioID, _, _, _, _)),
    asserta(usuario_corrente(UsuarioID, Senha, Nivel, Acertos, NovaListaSalvos)).

adicionar_nivel(NovoNivel) :-
    usuario_corrente(UsuarioID, Senha, _, Acertos, Salvos),
    retract(usuario(UsuarioID, _, _, _, _)),
    assertz(usuario(UsuarioID, Senha, NovoNivel, Acertos, Salvos)),
    retract(usuario_corrente(UsuarioID, _, _, _, _)),
    asserta(usuario_corrente(UsuarioID, Senha, NovoNivel, Acertos, Salvos)).

finalizar_sessao :-
    writeln('Salvando seu progresso...'),
    salva_usuarios,
    open('flashcards.pl', write, Stream),
    format(Stream, ':- module(flashcards, [flashcard/2]).~n~n', []),
    format(Stream, ':- dynamic flashcard/2.~n~n', []),
    close(Stream).
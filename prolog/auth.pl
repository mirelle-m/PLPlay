% AUTH.PL - VERSÃO REFATORADA PARA USUARIO/5

:- module(auth, [
    loop_autenticacao/0,
    login_corrente/5, % Mudou para 5 argumentos
    autenticar_ou_cadastrar/2,

    % Predicados renomeados e novos para clareza
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

% A aridade dos fatos dinâmicos mudou para 5
:- dynamic usuario/5.
:- dynamic usuario_corrente/5.

carrega_usuarios :-
    % Atualizado para o formato de 5 argumentos
    retractall(usuario(_,_,_,_,_)),
    (exists_file('usuarios.pl') -> consult('usuarios.pl');
        true).

salva_usuarios :-
    open('usuarios.pl', write, Stream),
    % Atualizado para salvar 5 argumentos
    forall(usuario(U,S,N,A,SV),
           format(Stream, 'usuario(~q, ~q, ~q, ~q, ~q).~n', [U,S,N,A,SV])),
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
    ( usuario(Username, StoredSenha, Nivel, Acertos, Salvos) ->
        (   Senha == StoredSenha ->
            writeln("✅ Autenticado com sucesso!"),
            retractall(usuario_corrente(_,_,_,_,_)),
            asserta(usuario_corrente(Username, Senha, Nivel, Acertos, Salvos)),
            writeln('Pressione ENTER para continuar...'),
            read_line_to_string(user_input, _)
        ;
            writeln("❌ Senha incorreta! Tente novamente!"),
            fail
        )
    ;
        % --- Cláusula de Novo Usuário ---
        writeln("✅ Usuário não encontrado. Cadastro realizado com sucesso!"),
        assertz(usuario(Username, Senha, "1", [], [])),
        retractall(usuario_corrente(_,_,_,_,_)),
        asserta(usuario_corrente(Username, Senha, "1", [], []))
    ).

% --- PREDICADOS DE ACESSO E MODIFICAÇÃO ATUALIZADOS ---

login_corrente(Username, Senha, Nivel, Acertos, Salvos) :-
    usuario_corrente(Username, Senha, Nivel, Acertos, Salvos).

% Getter específico para a lista de acertos
obter_questoes_acertadas(UsuarioID, Acertos) :-
    usuario_corrente(UsuarioID, _, _, Acertos, _). 

% Getter específico para a lista de questões salvas
obter_questoes_salvas(UsuarioID, Salvos) :-
    usuario_corrente(UsuarioID, _, _, _, Salvos).

% Getter para o nível (agora ignora dois argumentos de lista)
obter_progresso_nivel(UsuarioID, Nivel) :-
    usuario_corrente(UsuarioID, _, Nivel, _, _).

% Renomeado de adicionar_missao_concluida para mais clareza
adicionar_acerto(QuestaoID) :-
    usuario_corrente(UsuarioID, Senha, Nivel, AcertosAtuais, Salvos),
    (member(QuestaoID, AcertosAtuais) ->
        NovaListaAcertos = AcertosAtuais
    ;
        NovaListaAcertos = [QuestaoID | AcertosAtuais]
    ),
    retract(usuario(UsuarioID, _, _, _, _)),
    assertz(usuario(UsuarioID, Senha, Nivel, NovaListaAcertos, Salvos)),
    retract(usuario_corrente(UsuarioID, _, _, _, _)),
    asserta(usuario_corrente(UsuarioID, Senha, Nivel, NovaListaAcertos, Salvos)).

% NOVO PREDICADO para gerenciar a nova lista de questões salvas
adicionar_questao_salva(QuestaoID) :-
    usuario_corrente(UsuarioID, Senha, Nivel, Acertos, SalvosAtuais),
     (member(QuestaoID, SalvosAtuais) ->
        NovaListaSalvos = SalvosAtuais
    ;
        NovaListaSalvos = [QuestaoID | SalvosAtuais]
    ),
    retract(usuario(UsuarioID, _, _, _, _)),
    assertz(usuario(UsuarioID, Senha, Nivel, Acertos, NovaListaSalvos)),
    retract(usuario_corrente(UsuarioID, _, _, _, _)),
    asserta(usuario_corrente(UsuarioID, Senha, Nivel, Acertos, NovaListaSalvos)).

% Atualizado para manipular o fato de 5 argumentos
adicionar_nivel(NovoNivel) :-
    usuario_corrente(UsuarioID, Senha, _, Acertos, Salvos),
    retract(usuario(UsuarioID, _, _, _, _)),
    assertz(usuario(UsuarioID, Senha, NovoNivel, Acertos, Salvos)),
    retract(usuario_corrente(UsuarioID, _, _, _, _)),
    asserta(usuario_corrente(UsuarioID, Senha, NovoNivel, Acertos, Salvos)).

finalizar_sessao :-
    writeln('Salvando seu progresso...'),
    % 1. Salva o estado atual do usuário, sem modificar a lista de salvos.
    salva_usuarios,
    % 2. Abre o arquivo 'flashcards.pl' em modo de escrita e o fecha em seguida.
    %    Isso apaga todo o conteúdo do arquivo, mas mantém o arquivo no disco.
    writeln('Limpando o arquivo de flashcards para a próxima sessão...'),
    open('flashcards.pl', write, Stream),
    close(Stream).


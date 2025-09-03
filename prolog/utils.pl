:- module(utils, [mostrar_banner/1]).

mostrar_banner(Caminho) :-
    ( exists_file(Caminho) ->
        read_file_to_string(Caminho, Conteudo, []),
        split_string(Conteudo, "\n", "", Linhas),
        forall(member(Linha, Linhas), writeln(Linha))
    ; writeln("⚠️ Banner não encontrado!")
    ).

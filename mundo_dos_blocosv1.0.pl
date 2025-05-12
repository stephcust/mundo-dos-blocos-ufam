:- use_module(library(clpfd)).
:- use_module(library(heaps)).

% Definição dos blocos
bloco(a, 1).
bloco(b, 1).
bloco(c, 2).
bloco(d, 3).

% Definição das posições válidas (6x4)
posicao((X, Y)) :- between(1, 6, X), between(1, 4, Y).

% Estados de exemplo
estado1([ 
    occupied((1,1)), clear((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), clear((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), occupied((4,2)), clear((4,3)), clear((4,4)),
    clear((5,1)), occupied((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), occupied((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(4,1)), on(b,(6,1)), on(c,(1,1)), on(d,(4,2))]).

destino1([on(a,(5,1)), on(b,(6,1)), on(c,(3,1)), on(d,(4,2))]).

% Movimentos
acao(mover1(Bloco, De, Para)) :- bloco(Bloco, 1), posicao(De), posicao(Para), De \== Para.
acao(mover2(Bloco, De, Para)) :- bloco(Bloco, 2), posicao(De), posicao(Para), De \== Para.
acao(mover3(Bloco, De, Para)) :- bloco(Bloco, 3), posicao(De), posicao(Para), De \== Para.

% Regras de movimento
% Aqui foi utilizado a lógica de pode (can)
% Para mensurar se um bloco tem condições de ser movimentado da seguinte forma:
% Verifica se o destino é válido.
% Se não há blocos em cima.
% Se a nova posição está livre.
% Se a nova posição é estável.

pode(mover1(Bloco, De, Para), [on(Bloco,De)|Condicoes]) :-
    bloco(Bloco, 1), posicao(Para), estavel(Para, 1, ListaOc), posicao(De), De \== Para,
    \+ acima_de_si(De,1,ListaOc),
    cima_livre(De, 1, [], ListaLivre),
    append([clear(Para)|ListaLivre], ListaOc, Condicoes).

% Para bloco de tamanho 2
pode(mover2(Bloco, De, Para), [on(Bloco,De)|Condicoes]) :-
    bloco(Bloco, 2), regiao_valida(Para, 2), estavel(Para, 2, ListaOc), posicao(De), De \== Para,
    \+ acima_de_si(De,2,ListaOc),
    cima_livre(De, 2, [], CimaLivre),
    posicoes_livres(Para, 2, [], Livre2),
    append(CimaLivre, Livre2, ListaLivre),
    append(ListaLivre, ListaOc, Condicoes).

% Para bloco de tamanho 3
pode(mover3(Bloco, De, Para), [on(Bloco,De)|Condicoes]) :-
    bloco(Bloco, 3), regiao_valida(Para, 3), estavel(Para, 3, ListaOc), posicao(De), De \== Para,
    \+ acima_de_si(De,3,ListaOc),
    cima_livre(De, 3, [], CimaLivre),
    posicoes_livres(Para, 3, [], Livre2),
    append(CimaLivre, Livre2, ListaLivre),
    append(ListaLivre, ListaOc, Condicoes).

% Efeitos dos movimentos
% Os predicados "adiciona" listam os fatos que devem ser adicionados ao mundo ao realizar a ação.
adiciona(mover1(Bloco, De, Para), [on(Bloco, Para), occupied(Para), clear(De)]).
adiciona(mover2(Bloco, De, Para), [on(Bloco, Para)|Condicoes]) :-
    posicoes_ocupadas(Para, 2, [], ListaOc),
    posicoes_livres(De, 2, [], ListaLivre),
    append(ListaOc, ListaLivre, Condicoes).
adiciona(mover3(Bloco, De, Para), [on(Bloco, Para)|Condicoes]) :-
    posicoes_ocupadas(Para, 3, [], ListaOc),
    posicoes_livres(De, 3, [], ListaLivre),
    append(ListaOc, ListaLivre, Condicoes).

% Os predicados "remove" listam os fatos que devem ser removidos ao mundo ao realizar a ação.
remove(mover1(Bloco, De, Para), [on(Bloco, De), occupied(De), clear(Para)]).
remove(mover2(Bloco, De, Para), [on(Bloco, De)|Condicoes]) :-
    posicoes_ocupadas(De, 2, [], ListaOc),
    posicoes_livres(Para, 2, [], ListaLivre),
    append(ListaOc, ListaLivre, Condicoes).
remove(mover3(Bloco, De, Para), [on(Bloco, De)|Condicoes]) :-
    posicoes_ocupadas(De, 3, [], ListaOc),
    posicoes_livres(Para, 3, [], ListaLivre),
    append(ListaOc, ListaLivre, Condicoes).

% Predicados Auxiliares

% Verifica se há blocos empilhados acima da posição de origem.
acima_de_si((X,Y), 1, [occupied((X,Y))]).
acima_de_si((Xb,Y), 2, [occupied((X,Y)),occupied((X2,Y))]) :- Xb2 is Xb + 1, (Xb == X; Xb == X2; Xb2 == X).
acima_de_si((Xb,Y), 3, [occupied((X,Y))|_]) :- Xb2 is Xb + 1, Xb3 is Xb2 + 1, (Xb == X; Xb2 == X; Xb3 == X).

% Geram listas de posições ocupadas/livres com base no tamanho do bloco.
posicoes_ocupadas(_, 0, L, L).
posicoes_ocupadas((X,Y), Tam, L, Saida) :-
    Tam > 0.0, X2 #= X + 1, Tam2 is Tam - 1,
    posicoes_ocupadas((X2,Y), Tam2, [occupied((X,Y))|L], Saida).
posicoes_livres(_, 0, L, L).
posicoes_livres((X,Y), Tam, L, Saida) :-
    Tam > 0.0, X2 #= X + 1, Tam2 is Tam - 1,
    posicoes_livres((X2,Y), Tam2, [clear((X,Y))|L], Saida).

% Verifica se o bloco de tamanho Tam cabe a partir da posição (X,Y).
regiao_valida((X,Y), Tam) :- X2 #= X + Tam - 1, posicao((X2,Y)).

% Gera a lista de posições que precisam estar livres acima de um bloco para ele ser removido.
cima_livre(_, 0, L, L).
cima_livre((X,Y), Tam, L, Saida) :-
    Tam > 0.0, X2 #= X + 1, Y2 #= Y + 1, Tam2 is Tam - 1,
    cima_livre((X2,Y), Tam2, [clear((X,Y2))|L], Saida).

% Define se a posição de destino é estável, ou seja, se o bloco terá suporte embaixo para ser colocado.
estavel((X,Y), 1, [occupied((X,Y2))]) :- Y2 #= Y - 1.
estavel((X,Y), 2, [occupied((X,Y2)), occupied((X2,Y2))]) :- Y2 #= Y - 1, X2 #= X + 1.
estavel((X,Y), 3, [occupied((X2,Y2))]) :- Y2 #= Y - 1, X2 #= X + 1.
estavel((X,Y), 3, [occupied((X,Y2)), occupied((X2,Y2))]) :- Y2 #= Y - 1, X2 #= X + 2.

% Planejador

%Verificam se todas as metas estão satisfeitas no estado atual.
satisfeito(_, []).
satisfeito(Estado, [Meta|Metas]):- member(Meta, Estado), satisfeita(Estado, Metas).
satisfeita(Estado, Metas) :- satisfeito(Estado, Metas).

% Escolhe uma meta ainda não satisfeita.
seleciona_meta(Estado, Metas, Meta):- member(Meta, Metas), \+ member(Meta, Estado).

% Verifica se uma ação contribui para atingir uma meta.
realiza_acao(Acao, Meta):- adiciona(Acao, Metas), member(Meta, Metas).

% Aplica uma ação a um estado, gerando um novo estado.
aplica(Estado, Acao, NovoEstado):-
    remove(Acao, Remover), deleta_todos(Estado, Remover, Parcial),
    adiciona(Acao, Adicionar), append(Adicionar, Parcial, NovoEstado).

% Remove múltiplos fatos de uma lista.
deleta_todos([], _, []).
deleta_todos([X|L1], L2, Saida):- member(X, L2), !, deleta_todos(L1, L2, Saida).
deleta_todos([X|L1], L2, [X|Saida]):- deleta_todos(L1, L2, Saida).

% A*
busca_astar(EstadoInicial, Metas, Plano) :-
    empty_heap(HeapVazio),
    avaliacao_heuristica(EstadoInicial, Metas, H),
    add_to_heap(HeapVazio, H, no(EstadoInicial, [], 0, []), Heap),
    loop_astar(Heap, Metas, [], PlanoReverso),
    reverse(PlanoReverso, Plano).

loop_astar(Heap, Metas, _, Plano) :- get_from_heap(Heap, _, no(Estado, Plano, _, _), _), satisfeito(Estado, Metas), !.
loop_astar(Heap, Metas, Visitados, Solucao) :-
    get_from_heap(Heap, _, no(Estado, Plano, G, Caminho), RestoHeap),
    findall(h(F, no(NovoEstado, [Acao|Plano], G1, [Estado|Caminho])),
        (   acao(Acao), pode(Acao, Cond), satisfeito(Estado, Cond),
            aplica(Estado, Acao, NovoEstado),
            \+ membro_estado(NovoEstado, [Estado|Caminho]),
            \+ membro_estado(NovoEstado, Visitados),
            avaliacao_heuristica(NovoEstado, Metas, H),
            G1 is G + 1, F is G1 + H),
        Novos),
    adiciona_nos(RestoHeap, Novos, NovoHeap),
    loop_astar(NovoHeap, Metas, [Estado|Visitados], Solucao).

adiciona_nos(Heap, [], Heap).
adiciona_nos(Heap, [h(H,No)|Resto], HeapFinal) :- add_to_heap(Heap, H, No, ProxHeap), adiciona_nos(ProxHeap, Resto, HeapFinal).

membro_estado(Estado, [H|_]) :- iguais(Estado, H), !.
membro_estado(Estado, [_|T]) :- membro_estado(Estado, T).
membro_estado(_, []) :- fail.

iguais(A, B) :- msort(A, SA), msort(B, SB), SA == SB.

avaliacao_heuristica(Estado, Metas, H) :-
    length(Metas, L),
    findall(1, (member(on(B,P), Metas), \+ member(on(B,P), Estado)), Faltando),
    length(Faltando, H1),
    H is H1 + L.

% Teste de busca
mundo_dos_blocos :-
    estado1(Estado), destino1(Destino),
    statistics(runtime, [Inicio|_]),
    (busca_astar(Estado, Destino, Plano) ->
        statistics(runtime, [Fim|_]),
        Tempo is Fim - Inicio,
        format("Plano encontrado em ~w ms:\n~w\n", [Tempo, Plano])
    ;   writeln("Nenhum plano encontrado.")).

:- use_module(library(clpfd)).
:- use_module(library(heaps)).

% Definição dos blocos
bloco(a, 1).
bloco(b, 1).
bloco(c, 2).
bloco(d, 3).

% Definição das posições válidas (6x4)
posicao((X, Y)) :- between(1, 6, X), between(1, 4, Y).

% Descrição dos estados iniciais e metas a serem alcançadas

% Estado inicial i1
estado1([ 
    occupied((1,1)), clear((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), clear((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), occupied((4,2)), clear((4,3)), clear((4,4)),
    clear((5,1)), occupied((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), occupied((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(4,1)), on(b,(6,1)), on(c,(1,1)), on(d,(4,2))
]).

% Meta para alcançar i2. 
meta_i2([
    on(a,(1,2)), on(b,(6,1)), on(c,(1,1)), on(d,(3,1))
]).

% Estado inicial i2
estado2([
    occupied((1,1)), occupied((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), clear((2,2)), clear((2,3)), clear((2,4)),
    occupied((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), clear((4,2)), clear((4,3)), clear((4,4)),
    occupied((5,1)), clear((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), clear((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(1,2)), on(b,(6,1)), on(c,(1,1)), on(d,(3,1))
]).

% Meta para alcançar estado a
meta_a([
    on(a,(1,2)), on(b,(6,1)), on(c,(2,1)), on(d,(3,1))
]).

% Meta para alcançar estado b
meta_b([
    on(a,(5,3)), on(b,(6,3)), on(c,(5,2)), on(d,(4,1))
]).

% Meta para alcançar estado c
meta_c([
    on(a,(3,1)), on(b,(6,1)), on(c,(1,1)), on(d,(1,2))
]).

% Estado inicial para situação 2
situacao2([
    occupied((1,1)), occupied((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), occupied((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), clear((4,2)), clear((4,3)), clear((4,4)),
    occupied((5,1)), clear((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), clear((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(1,2)), on(b,(2,2)), on(c,(1,1)), on(d,(4,1))
]).

% Meta para alcançar o Sf para situação2
meta_situacao2([
    on(a,(5,3)), on(b,(6,3)), on(c,(5,2)), on(d,(4,1))
]).

% Estado inicial para as situação 3 e 4
situacao3e4([
    occupied((1,1)), clear((1,2)), clear((1,3)), clear((1,4)),
    occupied((2,1)), clear((2,2)), clear((2,3)), clear((2,4)),
    clear((3,1)), clear((3,2)), clear((3,3)), clear((3,4)),
    occupied((4,1)), occupied((4,2)), clear((4,3)), clear((4,4)),
    clear((5,1)), occupied((5,2)), clear((5,3)), clear((5,4)),
    occupied((6,1)), occupied((6,2)), clear((6,3)), clear((6,4)),
    occupied((1,0)), occupied((2,0)), occupied((3,0)), occupied((4,0)), 
    occupied((5,0)), occupied((6,0)),
    on(a,(4,1)), on(b,(6,1)), on(c,(1,1)), on(d,(4,2))
]).

% Meta para alcançar o Sf para situação 3
meta_situacao3([
    on(a,(5,2)), on(b,(3,3)), on(c,(3,2)), on(d,(3,1))
]).

% Meta para alcançar o Sf para situação 4
meta_situacao4([
    on(a,(5,1)), on(b,(6,1)), on(c,(3,1)), on(d,(4,2))
]).

% Movimentos
acao(mover1(Bloco, De, Para)) :- bloco(Bloco, 1), posicao(De), posicao(Para), De \== Para.
acao(mover2(Bloco, De, Para)) :- bloco(Bloco, 2), posicao(De), posicao(Para), De \== Para.
acao(mover3(Bloco, De, Para)) :- bloco(Bloco, 3), posicao(De), posicao(Para), De \== Para.

% Regras para realizar um movimento.
% Aqui foi utilizado a lógica de pode (can)
% Para mensurar se um bloco tem condições de ser movimentado da seguinte forma:
% Verifica se o destino é válido.
% Se não há blocos em cima.
% Se a nova posição está livre.
% Se a nova posição é estável.

pode(mover1(Bloco, De, Para), [on(Bloco,De)|Condicoes]) :-
    bloco(Bloco, 1),
    posicao(Para),
    estavel(Para, 1, ListaOcupados),
    posicao(De),
    De \== Para,
    \+ acima_de_si_mesmo(De,1,ListaOcupados),
    limpar_acima(De, 1, [], ListaLimpos),
    append([clear(Para)|ListaLimpos], ListaOcupados, Condicoes).

% Para bloco de tamanho 2
pode(mover2(Bloco, De, Para), [on(Bloco,De)|Condicoes]) :-
    bloco(Bloco, 2),
    regiao_valida(Para, 2),
    estavel(Para, 2, ListaOcupados),
    posicao(De),
    De \== Para,
    \+ acima_de_si_mesmo(De,2,ListaOcupados),
    limpar_acima(De, 2, [], ListaLimpos1),
    limpar_posicoes(Para, 2, [], ListaLimpos2),
    append(ListaLimpos1, ListaLimpos2, ListaLimpos),
    append(ListaLimpos, ListaOcupados, Condicoes).

% Para bloco de tamanho 3
pode(mover3(Bloco, De, Para), [on(Bloco,De)|Condicoes]) :-
    bloco(Bloco, 3),
    regiao_valida(Para, 3),
    estavel(Para, 3, ListaOcupados),
    posicao(De),
    De \== Para,
    \+ acima_de_si_mesmo(De,3,ListaOcupados),
    limpar_acima(De, 3, [], ListaLimpos1),
    limpar_posicoes(Para, 3, [], ListaLimpos2),
    append(ListaLimpos1, ListaLimpos2, ListaLimpos),
    append(ListaLimpos, ListaOcupados, Condicoes).

% Efeitos dos movimentos
% Os predicados "adiciona" listam os fatos que devem ser adicionados ao mundo ao realizar a ação.
adiciona(mover1(Bloco, De, Para), [on(Bloco, Para), occupied(Para), clear(De)]).
adiciona(mover2(Bloco, De, Para), [on(Bloco, Para)|Condicoes]) :-
    posicoes_ocupadas(Para, 2, [], ListaOcupados),
    limpar_posicoes(De, 2, [], ListaLimpos),
    append(ListaOcupados, ListaLimpos, Condicoes).
adiciona(mover3(Bloco, De, Para), [on(Bloco, Para)|Condicoes]) :-
    posicoes_ocupadas(Para, 3, [], ListaOcupados),
    limpar_posicoes(De, 3, [], ListaLimpos),
    append(ListaOcupados, ListaLimpos, Condicoes).

% Os predicados "remove" listam os fatos que devem ser removidos ao mundo ao realizar a ação.
remove(mover1(Bloco, De, Para), [on(Bloco, De), occupied(De), clear(Para)]).
remove(mover2(Bloco, De, Para), [on(Bloco, De)|Condicoes]) :-
    posicoes_ocupadas(De, 2, [], ListaOcupados),
    limpar_posicoes(Para, 2, [], ListaLimpos),
    append(ListaOcupados, ListaLimpos, Condicoes).
remove(mover3(Bloco, De, Para), [on(Bloco, De)|Condicoes]) :-
    posicoes_ocupadas(De, 3, [], ListaOcupados),
    limpar_posicoes(Para, 3, [], ListaLimpos),
    append(ListaOcupados, ListaLimpos, Condicoes).

% Predicados auxiliares do planejador
satisfeito(_, []).
satisfeito(Estado, [Meta|Metas]):-
    member(Meta, Estado),
    satisfeito(Estado, Metas).

selecionar(Estado, Metas, Meta):-
    member(Meta, Metas),
    \+ member(Meta, Estado).

alcanca(Acao, Meta):-
    adiciona(Acao, Metas),
    member(Meta, Metas).

aplicar(Estado, Acao, NovoEstado):-
    remove(Acao, ListaRemocao),
    deletar_todos(Estado, ListaRemocao, Estado1),
    adiciona(Acao, ListaAdicao),
    append(ListaAdicao, Estado1, NovoEstado).

deletar_todos([], _, []).
deletar_todos([X|L1], L2, Diferenca):-
    member(X, L2), !,
    deletar_todos(L1, L2, Diferenca).
deletar_todos([X|L1], L2, [X|Diferenca]):-
    deletar_todos(L1, L2, Diferenca).

% Predicados auxiliares
% Verifica se há blocos empilhados acima da posição de origem.
acima_de_si_mesmo((X,Y), 1, [occupied((X,Y))]).
acima_de_si_mesmo((Xb,Y), 2, [occupied((X,Y)),occupied((X2,Y))]) :-
    Xb2 is Xb + 1,
    (Xb == X; Xb == X2; Xb2 == X).
acima_de_si_mesmo((Xb,Y), 3, [occupied((X,Y))|_]) :-
    Xb2 is Xb + 1,
    Xb3 is Xb2 + 1,
    (Xb == X; Xb2 == X; Xb3 == X).

% Geram listas de posições ocupadas/livres com base no tamanho do bloco.
posicoes_ocupadas(_, 0, Lista, Lista).
posicoes_ocupadas((X,Y), Tamanho, Lista, ListaOcupados) :-
    Tamanho > 0,
    X2 #= X + 1,
    Tamanho2 is Tamanho - 1,
    posicoes_ocupadas((X2,Y), Tamanho2, [occupied((X,Y))|Lista], ListaOcupados).

limpar_posicoes(_, 0, Lista, Lista).
limpar_posicoes((X,Y), Tamanho, Lista, ListaLimpos) :-
    Tamanho > 0,
    X2 #= X + 1,
    Tamanho2 is Tamanho - 1,
    limpar_posicoes((X2,Y), Tamanho2, [clear((X,Y))|Lista], ListaLimpos).

% Verifica se o bloco de tamanho Tam cabe a partir da posição (X,Y).
regiao_valida((X, Y), Tamanho) :-
    X2 #= X + Tamanho - 1,
    posicao((X2, Y)).

limpar_acima(_, 0, Lista, Lista).
limpar_acima((X,Y), Tamanho, Lista, ListaLimpos) :-
    Tamanho > 0,
    X2 #= X + 1,
    Y2 #= Y + 1,
    Tamanho2 is Tamanho - 1,
    limpar_acima((X2,Y), Tamanho2, [clear((X,Y2))|Lista], ListaLimpos).
% Define se a posição de destino é estável, ou seja, se o bloco terá suporte embaixo para ser colocado.
estavel((X,Y), 1, [occupied((X,Y2))]) :- Y2 #= Y - 1.
estavel((X,Y), 2, [occupied((X,Y2)), occupied((X2,Y2))]) :- 
    Y2 #= Y - 1, 
    X2 #= X + 1.
estavel((X,Y), 3, [occupied((X2,Y2))]) :- 
    Y2 #= Y - 1, 
    X2 #= X + 1.
estavel((X,Y), 3, [occupied((X,Y2)), occupied((X2,Y2))]) :- 
    Y2 #= Y - 1, 
    X2 #= X + 2.


% ----------- HEURÍSTICAS -----------

% 1. Blocos fora do lugar
h_blocos_fora_do_lugar(Estado, Metas, Valor) :-
    findall(1, (member(on(Bloco,Pos), Metas), \+ member(on(Bloco,Pos), Estado)), L),
    length(L, Valor).

% 2. Pilhas intermediárias ocupadas (blocos em pilhas que não fazem parte da meta)
h_pilhas_intermediarias(Estado, Metas, Valor) :-
    findall(1, (
        member(on(Bloco, (X, _)), Estado),
        \+ (member(on(Bloco, (X, _)), Metas))
    ), L),
    length(L, Valor).

% 3. Distância Manhattan dos blocos
h_distancia_manhattan(Estado, Metas, Valor) :-
    findall(Dist, (
        member(on(Bloco,Pos1), Estado),
        member(on(Bloco,Pos2), Metas),
        Pos1 \= Pos2,
        Pos1 = (X1,Y1),
        Pos2 = (X2,Y2),
        Bloco \= _,
        Dist is abs(X2-X1) + abs(Y2-Y1)
    ), Distancias),
    sum_list(Distancias, Valor).

% 4. Blocos na pilha errada (coluna errada)
h_pilha_errada(Estado, Metas, Valor) :-
    findall(1, (
        member(on(Bloco, (X1, _)), Estado),
        member(on(Bloco, (X2, _)), Metas),
        X1 \= X2
    ), L),
    length(L, Valor).


% 5. Blocos bloqueados (há algo acima deles)
h_blocos_bloqueados(Estado, Metas, Valor) :-
    findall(1, (
        member(on(Bloco, (X, Y)), Estado),
        member(on(Bloco, PosMeta), Metas),
        (X, Y) \= PosMeta,
        Y1 is Y + 1,
        member(occupied((X, Y1)), Estado)
    ), L),
    length(L, Valor).

% 6. Blocos na ordem errada na pilha
h_ordem_errada(Estado, Metas, Valor) :-
    findall(1, (
        member(on(Bloco, (X, Y)), Estado),
        member(on(Bloco, (X, Yg)), Metas),
        X = X, % mesma pilha
        Y \= Yg
    ), L),
    length(L, Valor).


% 7. Blocos soltos (deveriam estar empilhados, mas estão sozinhos)
h_blocos_isolados(Estado, Metas, Valor) :-
    findall(1, (
        member(on(Bloco, (X, Y)), Estado),
        member(on(Bloco, (X, Yg)), Metas),
        Yg > 1, % deveria estar empilhado
        Y = 1   % está sozinho
    ), L),
    length(L, Valor).

% ----------- Função de avaliação combinada -----------

valor_h(Estado, Metas, H) :-
    h_blocos_fora_do_lugar(Estado, Metas, H1),
    h_distancia_manhattan(Estado, Metas, H2),
    h_blocos_bloqueados(Estado, Metas, H3),
    h_pilha_errada(Estado, Metas, H4),
    h_blocos_isolados(Estado, Metas, H5),
    h_ordem_errada(Estado, Metas, H6),
    h_pilhas_intermediarias(Estado, Metas, H7),
    % Ajuste os pesos conforme necessário
    H is 3*H1 + 2*H2 + 4*H3 + 2*H4 + 2*H5 + 2*H6 + 1*H7.

% ----------- BUSCA A* -----------

busca_a_estrela(EstadoInicial, Metas, Plano) :-
    empty_heap(HeapVazio),
    valor_h(EstadoInicial, Metas, H),
    add_to_heap(HeapVazio, H, node(EstadoInicial, [], 0, []), Heap),
    loop_busca_a_estrela(Heap, Metas, [], PlanoReverso),
    reverse(PlanoReverso, Plano).

loop_busca_a_estrela(Heap, Metas, _, Plano) :-
    get_from_heap(Heap, _, node(Estado, Plano, _, _), _),
    satisfeito(Estado, Metas), !.
loop_busca_a_estrela(Heap, Metas, Visitados, Solucao) :-
    get_from_heap(Heap, _, node(Estado, Plano, G, Caminho), RestoHeap),
    findall(h(F, node(NovoEstado, [Acao|Plano], G1, [Estado|Caminho])),
        (   acao_possivel(Estado, Acao),
            aplicar(Estado, Acao, NovoEstado),
            \+ estado_membro(NovoEstado, [Estado|Caminho]),
            \+ estado_membro(NovoEstado, Visitados),
            valor_h(NovoEstado, Metas, H),
            G1 is G + 1,
            F is G1 + H
        ),
        NovosNos),
    adicionar_nos_ao_heap(RestoHeap, NovosNos, NovoHeap),
    loop_busca_a_estrela(NovoHeap, Metas, [Estado|Visitados], Solucao).

adicionar_nos_ao_heap(Heap, [], Heap).
adicionar_nos_ao_heap(Heap, [h(H,No)|Resto], HeapFinal) :-
    add_to_heap(Heap, H, No, ProximoHeap),
    adicionar_nos_ao_heap(ProximoHeap, Resto, HeapFinal).

acao_possivel(Estado, Acao) :-
    acao(Acao), pode(Acao, Condicoes), satisfeito(Estado, Condicoes).

estado_membro(Estado, [H|_]) :- estados_iguais(Estado, H), !.
estado_membro(Estado, [_|T]) :- estado_membro(Estado, T).
estado_membro(_, []) :- fail.

estados_iguais(A, B) :-
    msort(A, SA), msort(B, SB), SA == SB.

% ----------- TESTE -----------

mundo_dos_blocos(EstadoInicial, Meta) :-
    call(EstadoInicial, S),
    call(Meta, G),
    statistics(runtime, [Inicio|_]),
    (busca_a_estrela(S, G, Plano) ->
        statistics(runtime, [Fim|_]),
        Tempo is Fim - Inicio,
        format('Plano encontrado de ~w para ~w em ~w ms:~n~w~n', 
               [EstadoInicial, Meta, Tempo, Plano])
    ;   format('Não foi possível encontrar um plano de ~w para ~w.~n', 
               [EstadoInicial, Meta])
    ).

% Exemplos de uso com os nomes em português:

% Testar com estado1 para estado2
% ?- mundo_dos_blocos(estado1, meta_i2).

% Testar com estado1 e meta_a
% ?- mundo_dos_blocos(estado1, meta_a).

% Testar com estado1 e meta_b
% ?- mundo_dos_blocos(estado1, meta_b).

% Testar com estado1 e meta_c
% ?- mundo_dos_blocos(estado1, meta_c).

% Testar com estado2 e meta_a
% ?- mundo_dos_blocos(estado2, meta_a).

% Testar com estado2 e meta_b
% ?- mundo_dos_blocos(estado2, meta_b).

% Testar com estado2 e meta_c
% ?- mundo_dos_blocos(estado2, meta_c).

% Testar com situacao2 e meta_situacao2
% ?- mundo_dos_blocos(situacao2, meta_situacao2).

% Testar com situacao3e4 e meta_situacao3
% ?- mundo_dos_blocos(situacao3e4, meta_situacao3).

% Testar com situacao3e4 e meta_situacao4
% ?- mundo_dos_blocos(situacao3e4, meta_situacao4).

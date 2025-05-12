# 🏗️🧱 Mundo dos Blocos - Inteligência Artificial
**📘 1º Trabalho Prático da disciplina de Inteligência Artificial**

**👨‍🏫 Professor: Edjard Mota**

**👥 Integrantes**
  - Daniel Silveira Gonzalez
  - Júlio Melo Campos
  - Stepheson Custódio

## ℹ️ Descrição
Este projeto consiste em um **planejador de ações** para empilhar blocos de diferentes dimensões no contexto do **Mundo dos Blocos**. Nesse cenário, os blocos podem ser movidos de posição e empilhados uns sobre os outros seguindo critérios específicos.

A representação do mundo é feita por meio de um conjunto de **predicados**, que definem:
- O estado inicial do mundo;
- As ações possíveis;
- Os objetivos a serem alcançados.
---

## 🧠 Conceitos Aplicados

- Representação de estados com predicados;
- Planejamento baseado em IA simbólica;
- Manipulação de estruturas de dados para resolver problemas de busca.

---

## </> Desenvolvimento do Projeto

Inicialmente, foi desenvolvida a versão 1.0 do problema do Mundo dos Blocos, na qual foi utilizada uma **restrição universal** cuja ideia foi buscar um  de priorização dos estados a serem expandidos, permitindo que o algoritmo explore primeiro os caminhos mais promissores.

Contudo, essa heurística não restringe bem e resulta em uma busca de A* "aleatória" no âmbiente na reorganização dos blocos. Por isso, foi criada outra versão final na qual são utilizadas **restrições estratégicas dos blocos** que não impedem diretamente as ações, mas penalizam estados menos promissores, orientando o algoritmo A* a tomar decisões mais inteligentes.

---


## 🚀 Como executar

_Instruções:_
Para executar cada cenário descrito pelo livro do Bratko e no arquivo T1_MundoDosBlocos.pdf, basta colocar no terminal do arquivo mundo_dos_blocosvfinal.pl


OBS: Todos os casos já estão comentados no arquivo a partir da linha 392. 
```bash
# mundo_dos_blocos(estadoInicial, meta).

# Exemplo: Testar com estado1 e meta_c
# Estado Inicial
#                d d d
#          c c   a   b
#          - - - - - -
# posição  1 2 3 4 5 6

# Meta
#          d d d
#          c c a     b
#          - - - - - -
# posição  1 2 3 4 5 6

mundo_dos_blocos(estado1, meta_c).
```
   

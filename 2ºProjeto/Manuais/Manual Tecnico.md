# Manual de Utilizador

<center>

## **Dots & Boxes**

![Dots & Boxes](Dots%26Boxes.png)

>IPS ESTS - Licenciatura Engenharia Informatica - 2022/2023
>
>Inteligência Artificial
>\
>Docente: Joaquim Filipe
>
>Trabalho realizado por:
>
>Nome: Daniel Baptista - Nº:202001990
>\
>Nome: Rafael Silva - Nº:202001553
>

</center>

---
<div style="page-break-after: always; break-after: page;"></div>

## 2. Entidades e sua implementação

# REVER!!

* **_no_** - entidade que representa o ponto do tabuleiro de jogo.
* **_no-estado_** - entidade que representa o estado em que se encontra o ponto do tabuleiro de jogo.
* **_sucessores_** - entidade que representa todos os sucessores de um nó atual.
* **_abertos_** - entidade que representa a lista dos nós que podem ser expandidos.
* **_fechados_** - entidade que representa a lista dos nós que já foram percorridos e que tem sucessores.

## 3. Algoritmos e sua implementação

### **Algoritmo MiniMax com cortes alfa-beta**

É um algoritmo que faz a procura atravésdo tipo _depth-first_, pelo que em cada instante apenas é necessário considerar os nós ao longo de um ramo da árvore de procura.
\
Seja **α** o valor da melhor escolha encontrada até ao momento, ao longo do ramo corrente, para um nó **MAX**.
\
Seja **β** o valor da melhor escolha encontrada até ao momento, ao longo do ramo corrente, para um nó **MIN**.
\
Este vai atualizando o valor de **α** e **β** ao longo da procura podendo ou não cortar subárvores dependente se se sabe que os valores correntes sejam piores dos que os que já temos.
\
A complexidade algoritmica é de $O(b^{m/2})$, em que **$m$** é a profundidade máxima e **$b$** o fator de ramificação.

#### **Possivel forma de implementação do Alfa-Beta**

1. 

#### **Características do MiniMax com cortes alfa-beta**

* Examina menos nós do que o MiniMax devido aos cortes.
* Não altera os resultados finais em relação ao NegaMax ou MiniMax.
* É mais rápido para fazer a procura em relação ao MiniMax.

#### **Implementação feita no projeto**

```lisp

```

## 4. Descrição das opções tomadas

Uma das decisões tomadas foi a troca de variaveis globais de **α** e **β**, sugerida pelo professor, para closures de modo a não infrigir-mos qualquer propriedade da programação funcional. De resto foi feito a implementação através da logica dos ultimos laboratorios disponibilizados na cadeira.

## 5. Limitações técnicas e ideias para desenvolvimento futuro

Limitações técnicas do programa:

* Não mostrar o tempo decorrido do jogo.

Ideias para desenvolvimento futuro poderiam ser:

* Refactoring de modo a obter um maior nivel de abstração.
* Possiveis melhoramentos no desempenho dos algoritmos de procura.

<script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({ tex2jax: {inlineMath: [['$', '$']]}, messageStyle: "none" });
</script>

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

## 1. Acrónimos e Convenções usadas

* **Tabuleiro** - Onde o jogo é realizado.
  * Este é constituido por:
    * **$n$** -> Linhas de caixas.
    * **$m$** -> Colunas de caixas.
* **Caixa Fechada** - É um objeto no tabuleiro constituído por 4 pontos ligados por 4 arcos, sendo o objetivo do jogo ter estas caixas.
* **Ponto** - Pontos do tabuleiro
  * No total no tabuleiro existem $(n + 1) * (m + 1)$ pontos.
* **Arco** - É a ligação entre dois pontos adjacentes
  * No total no tabuleiro existem $(m *(n + 1)) + (n* (m + 1))$ arcos.
* **Profundidade** - É o nível em que se encontra um determinado nó.
* **Função de utilidade** - É o processo de procura da melhor jogada através de conhecimento prévio.

## 2. Introdução

No âmbito da cadeira de Inteligência Artificial foi realizado um projeto em **_Common Lisp_** de modo a colocar os conhecimentos adquiridos na parte teorica da cadeira a prova. O desafio proposto, pelos docentes da cadeira, para este projeto foi a resolução do jogo "**_Dots and Boxes_**".

O jogo consiste num jogo entre 2 jogadores, não-cooperativo, é um jogo sequencial de soma zero, em que tem como objetivo final obter o número máximo de caixas fechadas no tabuleiro de jogo.

Nesta 2ª fase do projeto foi pedido para fazer a implementação do jogo completo em que poderá ser jogado entre 1 pessoa e o computador ou então entre 2 computadores. Desta forma o objetivo torna-se para que o computador vença o jogador humano ou outro computador.

## 3. Instalação e utilização

Para utilizar o programa é necessária realizar os seguintes passos:

**1.** Ter o LispWorks instalado, caso não o tenha pode obte-lo aqui [LispWorks](http://wwwlispworkscomdownloadsindex.html)

**2.** Colocar os ficheiros da aplicação dentro de uma só pasta.

**3.** De seguida abrir o ficheiro projeto.lisp no LispWorks através do atalho no canto superior esquerdo "Open file" com o icon seguinte <img src="Open_file_icon.jpg" width="20" height="20">

**4.** Com o botão direito do rato na janela do Editor, selecionar a opção _Buffer_ e de seguida a opção _Evaluate_.

**5.** Depois executar a função iniciar no listener do LispWorks desta forma: ```(iniciar)```

**6.** Escolher a diretoria onde estão os ficheiros da aplicação. Exemplo: ```"C:\Users\username\Docs\Dots-Boxes_IA"```

**7.** Escolher o modo de jogo que queira fazer entre as 2 opções disponibilizadas:

* Computador vs Humano
* Computador vs Computador

**8.** Escolher a profundidade limite que irá ser percorrido o algoritmo.

**9.** Escolher qual o jogador que joga primeiro, isto se a escolha do modo de jogo seja a 1ª opção, entre 2 opções:

* Computador
* Humano

**10.** Escolher qual o tipo de arco que irá querer colocar entre:

* **Arco Horizontal**
* **Arco Vertical**

**11.** Escolher a linha em que deseja inserir o arco.

**12.** Escolher a coluna em que deseja inserir o arco.

## 4. Input/Output  

### **Input**

* **Tabuleiro** - É a representação de um tabuleiro que este é constituido por:
  * **Listas horizontais** - Que são ligações entre pontos adjacentes horizontalmente, sendo que 0 significa que não há arco entre dois pontos e 1 significa que existe um arco entre dois pontos.
  * **Listas verticais** - Que são ligações entre pontos adjacentes verticalmente, sendo que 0 significa que não há arco entre dois pontos e 1 significa que existe um arco entre dois pontos.
* **Profundidade limite** - É o limite verticalmente até onde o algoritmo **MiniMax com cortes Alfa-beta** irá fazer a procura dos nós, ou seja, é o nivel de profundidade até onde se quer que o algoritmo **MiniMax com cortes Alfa-beta** pesquise por uma solução.
* **Função de utilidade** - Faz a avaliação do estado do jogo de um tabuleiro em que:
  * Pontua +10 por cada caixa do MAX(Computador)
  * Pontua -10 por cada caixa do MIN(Adversario)
  * Pontua +5 por cada caixa com 3 lados fechados
  * Pontua -5 por cada caixa com 2 lados fechados

### **Output**

* **Estado atual** - Mostra o estado atual do tabuleiro após cada jogada.
* **Jogador vencedor** - Qual o jogador vencedor do jogo.
* **Jogada efetuada** - Quais os parametros usados para a jogada feita, que é constituida por:
  * **Linha escolhida** - Número da linha que o jogador selecionou para inserir o arco pretendido.
  * **Coluna escolhida** - Número da coluna que o jogador selecionou para inserir o arco pretendido.
  * **Operação escolhida** - Qual o arco que o jogador colocou no tabuleiro para efetuar a sua jogada.
* **Número de nós analisados** - Número de nós que o computador analisou para chegar a sua jogada.
* **Número de cortes alfa** - Número de nós que foram cortados pelo computador através de cortes por $\alpha$
* **Número de cortes beta** - Número de nós que foram cortados pelo computador através de cortes por $\beta$

## 5. Exemplo de aplicação

1. O utilizador deverá executar a função iniciar como já referido anteriormente.

<center>
  <figure>
    <img src="Ecrã inicial.png">
       <figcaption> Ecrã Inicial </figcaption>
  </figure>
</center>

2. O utilizador deverá de seguida escolher a diretoria onde residem os ficheiros da aplicação.

<center>
  <figure>
    <img src="Escolha da diretoria.png">
       <figcaption> Ecrã de Escolha de diretoria </figcaption>
  </figure>
</center>

3. Após a escolha da diretoria o utilizador terá que escolher qual o modo de jogo que pretende jogar.

<center>
  <figure>
    <img src="Escolha do modo.png">
       <figcaption> Ecrã de Escolha do modo de jogo que pretende jogar </figcaption>
  </figure>
</center>

4. De seguida o utilizador escolherá qual a profundidade maxima do tabuleiro de jogo.

<center>
  <figure>
    <img src="Escolha de profundidade limite.png">
       <figcaption> Ecrã de Escolha da profundidade maxima </figcaption>
  </figure>
</center>

5. Após ter selecionado a profundidade maxima irá escolher-se qual o jogador a começar em 1º lugar.

<center>
  <figure>
    <img src="Escolha do 1ª a jogar.png">
       <figcaption> Ecrã de Escolha do 1º jogador </figcaption>
  </figure>
</center>

6. Escolher qual o tipo de arco que pretende inserir, caso seja a 1ª pessoa a jogar.

<center>
  <figure>
    <img src="Escolha do tipo de arco a introduzir.png">
       <figcaption> Ecrã de Escolha do tipo de arco a inserir </figcaption>
  </figure>
</center>

7. Após ter selecionado o tipo de arco pretendido será pedido as coordenadas do mesmo, mais concretamente em que linha quer inserir.

<center>
  <figure>
    <img src="Escolha da linha que quer introduzir o arco.png">
       <figcaption> Ecrã de Escolha da linha a inserir o arco selecionado </figcaption>
  </figure>
</center>

8. Ainda para as coordenadas do arco é necessario escolher a coluna em que o mesmo será introduzido.

<center>
  <figure>
    <img src="Escolha da coluna que quer introduzir o arco.png">
       <figcaption> Ecrã de Escolha da coluna a inserir o arco selecionado </figcaption>
  </figure>
</center>

9. Por fim retorna o tabuleiro com a jogada efetuada pelo utilizador

<center>
  <figure>
    <img src="Retorno do tabuleiro após colocar o arco.png">
       <figcaption> Ecrã de Retorno do tabuleiro com a jogada indicada </figcaption>
  </figure>
</center>

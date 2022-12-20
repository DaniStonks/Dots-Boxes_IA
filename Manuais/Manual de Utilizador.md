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
* **Arco** - É a ligação entre dois pontos próximos
  * No total no tabuleiro existem $(m *(n + 1)) + (n* (m + 1))$ arcos.
* **Jogada** - É o turno do jogador em que escolhe onde vai colocar o seu próximo arco.
* **Profundidade** - É o número de níveis que foi necessário o algoritmo percorrer para encontrar o nó desejado.
* **Heurística** - É o processo de ajuda à procura de soluções usando conhecimento prévio.

## 2. Introdução

## 3. Instalação e utilização

Para utilizar o programa é necessária realizar os seguintes passos:

* Ter o LispWorks instalado
  * Caso não o tenha, ir ao [LispWorks](http://wwwlispworkscomdownloadsindex.html) e seguir os passos indicados.
* Ter os ficheiros da aplicação dentro de uma única pasta
  * projeto.lisp
  * puzzle.lisp
  * procura.lisp
* No LispWorks terá que abrir os três ficheiros anteriores com a opção "Open" localizada no "File".
* No ficheiro projeto.lisp é necessário fazer a alteração da diretoria onde os três ficheiros entitulados acima estão localizados.
  * Exemplo -> ```"C:\\Exemplo\\Utilizador\\Desktop\\...\\puzzle.lisp"```
* Fazer a realização do evaluate de cada ficheiro dentro do LispWorks
  * Clicar com o botão direito do rato no ficheiro
    * Selecionar a opção "Buffer"
    * Selecionar a opção "Evaluate"
* De seguida executar a função iniciar no listener da seguinte forma :

```lisp
(iniciar)
```

* Por fim escolher a opção que deseje do menu

## 5. Input/Output  

## 6. Exemplo de aplicação

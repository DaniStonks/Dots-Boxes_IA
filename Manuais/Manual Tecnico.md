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

## 1. Arquitetura do sistema

### **Módulo de Procura - procura.lisp**

É o módulo da aplicação onde estão localizadas todas as funções usadas pelos algoritmos e a implementação dos proprios algoritmos e este módulo pode ser usado genericamente por outros módulos.

#### **Têm como Objetivo**

Ser utilizado como biblioteca generica onde estão presentes os três algoritmos implementados (**BFS, DFS e A***), ou seja, poderá ser utilizado noutro problema qualquer que necessite o uso de um destes algoritmos.

#### **Conexões**

...

#### **Relacionamento entre as conexões**

...

### **Módulo da Interface com utilizador - projeto.lisp**

É o módulo da aplicação em que é criada a interface para a interação com o utilizador e ainda a leitura e escrita de feicheiros externos.

#### **Têm como Objetivo**

Ser utilizado para fazer a ligação entre o programa e o utilizador através de uma interface gráfica.

#### **Conexões**

...

#### **Relacionamento entre as conexões**

...

### **Módulo do Problema - puzzle.lisp**

É o módulo da aplicação onde existe uma .

#### **Têm como Objetivo**

Ser utilizado .

#### **Conexões**

...

#### **Relacionamento entre as conexões**

...

## 2. Entidades e sua implementação

– poderá recorrer-se à identificação e descrição de tipos abstratos de dados, objetos, ou usar outras formas de explicação  
– convém fazer um paralelo entre as entidades do domínio de aplicação e as entidades programáticas, sem as confundir

## 3. Algoritmos e sua implementação

### **Algoritmo BFS**

É um algoritmo que faz a procura de cima para baixo e de nivel a nivel até que seja encontrada a solução final ou a lista de abertos ficar vazia e neste caso significa que não existe uma solução possível. Visto isto conseguimos concluir que o algoritmo chega sempre a uma solução possivel caso esta exista e tambem consegue garantir que esta solução seja a solução ótima do problema.
\
A complexidade algoritmica é de $O(b^d)$, em que **$d$** é a profundidade máxima e **$b$** o fator de ramificação médio.

#### **Possivel forma de implementação do BFS**

1. Nó inicial => **Abertos**.
2. Se **Abertos** vazia falha.
3. Remove o primeiro nó de **Abertos** $(n)$ e coloca-o em **Fechados**.
4. Expande o nó $n$. Coloca os sucessores no fim dos **Abertos**, colondo ponteiros para $n$.
5. Se algum dos sucessores é um nó objectivo sai, e dá a solução, caso contrario volta a refazer os passos todos apartir do passo 2.

#### **Características do BFS**

* Assume-se que o nó inicial não é um nó objectivo.
* Este encontra sempre a solução com o caminho mais pequeno, ou seja, encontra sempre a solução ótima.
* Este termina com falha se não existir solução num grafo finito ou se o grafo for infinito este irá ficar num loop infinito não havendo um termino do mesmo.

#### **Implementação feita no projeto**

```lisp
;;procura na largura
(defun bfs (no f-objetivo f-sucessores operadores &optional num-solucao abertos fechados)
  (let* ((novos-fechados (cons no fechados))
        (novos-abertos (abertos-bfs abertos novos-fechados (funcall f-sucessores no operadores 'bfs nil)))
        (no-solucao (remove nil (mapcar (lambda (no)
                                              (cond((funcall f-objetivo no num-solucao) no)
                                                   (t NIL)))
                                            novos-abertos))))
    (setf *abertos* novos-abertos)
    (setf *fechados* novos-fechados)
    (cond ((null novos-abertos) NIL)
          ((funcall f-objetivo no num-solucao) no)
          ((not (null no-solucao)) (cond ((/= (length no-solucao) 1) (car no-solucao))
                                         (t (car no-solucao))))
          (t (bfs (car *abertos*) f-objetivo f-sucessores operadores num-solucao (cdr *abertos*) *fechados*)))))
```

### **Algoritmo DFS**

É um algoritmo que faz a procura de por ramos percorrendo-os verticalmente, neste algoritmo temos que definir a profundidade que pretendemos que este faça a pesquisa e dentro de um ramo se chegar ao fim e não tiver encontrado nenhuma solução vai percorrer o outro ramo seguinte até que encontre a solução. Visto isto pode-se concluir que se este não encontrar a solução no primero ramo que faça a pesquisa poderá tornar-se menos eficiente do que o algortimo BFS.
\
A complexidade algoritmica é de $O(b^d)$, em que **$d$** é a profundidade máxima e **$b$** o fator de ramificação médio.

#### **Possivel forma de implementação do DFS**

1. Nó inicial => **Abertos**.
2. Se **Abertos** vazia falha.
3. Remove o primeiro nó de **Abertos** $(n)$ e coloca-o em **Fechados**.
4. Se a profundidade de $n$ é maior do que $d$ (profundidade máxima) volta para o passo 2.
5. Expande o nó $n$. Coloca os sucessores no início dos **Abertos**, colocando ponteiros para $n$.
6. Se algum dos sucessores é um nó objectivo termina e dá a solução, se não volta a fazer os passos todos apartir do passo 2.

#### **Características do DFS**

* Defini-se um nível de profundidade máximo e apartir do mesmo não são gerados mais nós.

#### **Implementação feita no projeto**

```lisp
;; procura na profundidade
(defun dfs (no f-objetivo f-sucessores operadores profundidade &optional num-solucao abertos fechados)
  (let* ((novos-sucessores-e-fechados (abertos-dfs abertos (cons no fechados) (funcall f-sucessores no operadores 'dfs nil 0 profundidade)))
         (novos-abertos (car novos-sucessores-e-fechados))
         (novos-fechados (car (cdr novos-sucessores-e-fechados)))
         (no-solucao (remove nil (mapcar (lambda (no)
                                               (cond((funcall f-objetivo no num-solucao) no)
                                                    (t NIL)))
                                             novos-abertos))))
    (setf *abertos* novos-abertos)
    (setf *fechados* novos-fechados)
    (cond ((null novos-abertos) NIL)
          ((funcall f-objetivo no num-solucao) no)
          ((not (null no-solucao)) (cond ((/= (length no-solucao) 1) (car no-solucao))
                                         (t (car no-solucao))))
          (t (dfs (car *abertos*) f-objetivo f-sucessores operadores profundidade num-solucao (cdr *abertos*) *fechados*)))))
```

### **Algoritmo A***

É um algoritmo que se diz "_informado_" ao contrário dos outros dois algoritmos mencionados, este algoritmo usa metodos heurísticos para encontrar uma solução ótima da maneira mais eficiente possível.
\


#### **Possivel forma de implementação do A***

1. Nó inicial => **Abertos**. Faz $f(s)=0$.
2. Se **Abertos** vazia falha.
3. Remove o nó de **Abertos** $(n)$ com menor custo $(f)$ e coloca-o em **Fechados**.
4. Expande o nó $n$ e calcula o $f()$ de cada um dos sucessores.
5. Coloca os sucessores que ainda não existem em **Abertos** nem em **Fechados** na lista de **Abertos**, por ordem de $f()$ colocando ponteiros para $n$.
6. Se algum dos sucessor for um nó objectivo termina e devolve a solução.
7. Associa aos sucessores já em **Abertos** ou **Fechados** o menor dos valores de $f$ (existente ou agora calculado), coloca nos **Abertos** os sucessores que estavam em **Fechados** cujos valores de $f()$ baixaram, redirecionando para $n$ os ponteiros de todos os nós cujos valores de $f()$ baixaram.
8. Volta a fazer todos os passos apartir do passo 2.

#### **Características do A***

* 

#### **Implementação feita no projeto**

```lisp
;;procura informada
(defun a* (no f-solucao f-sucessores operadores f-heuristica &optional num-solucao abertos fechados)
  (let* ((novos-abertos-fechados (abertos-e-fechados-a* abertos (cons no fechados) (funcall f-sucessores no operadores 'a* f-heuristica num-solucao)))
         (novos-abertos (car novos-abertos-fechados))
         (novos-fechados (car (cdr novos-abertos-fechados)))
         (no-solucao (cond((funcall f-solucao (car novos-abertos) num-solucao) (car novos-abertos))
                          (t NIL))))
    (setf *abertos* novos-abertos)
    (setf *fechados* novos-fechados)
    (cond ((null novos-abertos) NIL)
          ((funcall f-solucao no num-solucao) no)
          ((not (null no-solucao)) no-solucao)
          (t (a* (car *abertos*) f-solucao f-sucessores operadores f-heuristica num-solucao (cdr *abertos*) *fechados*)))))
```

## 4. Descrição das opções tomadas

– descrever as opções de implementação que foram tomadas em detrimento de outras, por vezes essas opções podem não ser as mais óbvias e como tal devem ser documentadas

## 5. Limitações técnicas e ideias para desenvolvimento futuro
  
– requisitos não implementados  
– refactoring que se percebe ser necessário fazer no futuro mas que não houve tempo para fazer  
– melhoramentos potenciais de desempenho 
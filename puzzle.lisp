;;;; puzzle.lisp
;;;; Funcoes especificas ao dominio do jogo Dots and Boxes
;;;; Autor: Daniel Baptista, Rafael Silva


;;; Tabuleiro
  "Cria um nó do problema com um tabuleiro e opcionalmente profundidade, valor heuristico e nó pai"
(defun cria-no (tabuleiro &optional (g 0) (h 0) (pai nil))
  (list tabuleiro g h pai)
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
  '((
    ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
    ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
    ) 0 0 NIL))

;;Seletores
(defun no-estado (no)
  (first no))

(defun get-arcos-horizontais (tabuleiro-estado)
  "Retorna os arcos horizontais de um tabuleiro"
  (first tabuleiro-estado))

(defun get-arcos-verticais (tabuleiro-estado)
  "Retorna os arcos verticais de um tabuleiro"
  (second tabuleiro-estado))

(defun get-arco-na-posicao (pos-lista-arcos pos-arco arcos)
  "Retorna o arco numa posicao passada por argumento de uma lista de arcos horizontais ou verticais"
  (nth (1- pos-arco) (nth (1- pos-lista-arcos) arcos)))

(defun no-profundidade (no)
  (second no))

(defun no-heuristica (no)
  (third no))

(defun no-pai (no)
  (fourth no))

(defun no-custo (no)
  (+ (third no) (second no)))


;;Funcoes Auxiliares
(defun substituir (indice lista &optional (x 1))
  "Dada uma lista e um indice, substitui o valor nessa posicao por outro passado por argumento"
  (cond ((null lista) nil)
        ((= (1- indice) 0) (cons x (substituir (1- indice) (cdr lista) x)))
        (t (cons (car lista) (substituir (1- indice) (cdr lista) x)))))

(defun arco-na-posicao (pos-lista-arcos pos-arco lista-arcos &optional (x 1))
  "Insere um arco nos arcos horizontais ou verticais de um tabuleiro, na posicao escolhida"
  (cond ((= (1- pos-lista-arcos) 0) (cons (substituir pos-arco (car lista-arcos) x) (cdr lista-arcos)))
        (t (cons (car lista-arcos) (arco-na-posicao (1- pos-lista-arcos) pos-arco (cdr lista-arcos) x)))))


;;Operadores
(defun operadores ()
 "Cria uma lista com todos os operadores do problema das vasilhas."
 (list 'arco-horizontal 'arco-vertical))

(defun arco-horizontal (pos-lista-arcos pos-arco tabuleiro-estado &optional (x 1))
  "Coloca um arco horizontal num tabuleiro, na posicao passada por argumento"
  (let* ((arcos-hor (car tabuleiro-estado))
        (linhas (length arcos-hor))
        (colunas (length (car arcos-hor))))
    (cond ((or (> pos-lista-arcos linhas)(> pos-arco colunas)) NIL)
          ((= 1 (get-arco-na-posicao pos-lista-arcos pos-arco arcos-hor)) NIL)
          (t (cons (arco-na-posicao pos-lista-arcos pos-arco arcos-hor x) (cdr tabuleiro-estado))))))

(defun arco-vertical (pos-lista-arcos pos-arco tabuleiro-estado &optional (x 1))
  "Coloca um arco vertical num tabuleiro, na posicao passada por argumento"
  (let* ((arcos-ver (cadr tabuleiro-estado))
        (linhas (length arcos-ver))
        (colunas (length (car arcos-ver))))
    (cond ((or (> pos-lista-arcos linhas)(> pos-arco colunas)) NIL)
          ((= 1 (get-arco-na-posicao pos-lista-arcos pos-arco arcos-ver)) NIL)
          (t (cons (car tabuleiro-estado) (cons (arco-na-posicao pos-lista-arcos pos-arco arcos-ver x) nil))))))

(defun no-solucaop (no num-caixas-solucao)
  (let ((num-caixas-fechadas (contar-caixas-fechadas (no-estado no))))
    (cond ((= num-caixas-solucao num-caixas-fechadas) T)
          (t NIL))))

;;Teste: (contar-caixas-fechadas (no-estado (tabuleiro-teste)))
;;Resultado: 1
(defun contar-caixas-fechadas (estado &optional (l 1) (i 1))
  (let* ((arcos-hor (get-arcos-horizontais estado))
         (arcos-vert (get-arcos-verticais estado))
         (num-total-arcos-hor (1- (length arcos-hor)))
         (num-total-indices-hor (length (car arcos-hor))))
    (labels ((iterar-tabuleiro (l i)
               (cond ((> l num-total-arcos-hor) 0)
                     (t (let ((caixap (caixa-fechadap arcos-hor arcos-vert l i)))
                          (cond ((and caixap (= i num-total-indices-hor)) (+ 1 (iterar-tabuleiro (1+ l) 1)))
                                (caixap (+ 1 (iterar-tabuleiro l (1+ i))))
                                ((= i num-total-indices-hor) (iterar-tabuleiro (1+ l) 1))
                                (t (iterar-tabuleiro l (1+ i)))))))))
      (iterar-tabuleiro l i))))

;;Teste: (caixa-fechadap (get-arcos-horizontais (no-estado (tabuleiro-teste))) (get-arcos-verticais (no-estado(tabuleiro-teste))) 3 3)
;;Resultado: T
(defun caixa-fechadap(arcos-hor arcos-vert l i)
  (cond ((and (= 1 (get-arco-na-posicao l i arcos-hor))(= 1 (get-arco-na-posicao (1+ l) i arcos-hor))
              (= 1 (get-arco-na-posicao i l arcos-vert))(= 1 (get-arco-na-posicao (1+ i) l arcos-vert))) T)
        (t NIL)))

(defun heuristica (estado num-caixas-a-fechar)
  (- num-caixas-a-fechar (contar-caixas-fechadas estado)))

;;; sucessores
(defun novo-sucessor (no l i operador &optional (heuristica nil) (num-solucao 0))
  (let* ((novo-estado (funcall operador l i (no-estado no) 1))
        (nova-profundidade (1+ (second no)))
        (valor-heuristica (cond ((or(null heuristica)(null novo-estado)) 0)
                                (t (funcall heuristica novo-estado num-solucao)))))
    (list novo-estado nova-profundidade valor-heuristica no)))

;;Teste: (sucessores (tabuleiro-teste) (operadores) 'a* 'heuristica 5)
(defun sucessores (no operadores algoritmo &optional (heuristica nil) (num-solucao 0) profundidade)
  (cond ((and (eq algoritmo 'dfs) (= (second no) profundidade)) NIL)
        (t (let* ((arcos-hor (get-arcos-horizontais (no-estado no)))
                  (arcos-vert (get-arcos-verticais (no-estado no)))
                  (l-maximo (max (length arcos-hor) (length arcos-vert)))
                  (i-maximo (max (length (car arcos-hor)) (length (car arcos-vert)))))
             (labels ((iterar-tabuleiro (l i)
                        (cond ((> l l-maximo) NIL)
                              ((> i i-maximo) (iterar-tabuleiro (1+ l) 1))
                              (t (append (mapcar (lambda (op) (novo-sucessor no l i op heuristica num-solucao)) operadores) (iterar-tabuleiro l (1+ i)))))))
               (iterar-tabuleiro 1 1))))))
;;;; puzzle.lisp
;;;; Funcoes especificas ao dominio do jogo Dots and Boxes
;;;; Autor: Daniel Baptista, Rafael Silva


;;; Tabuleiro
  "Cria um não do problema com um tabuleiro e opcionalmente profundidade, valor heuristico e nÃ³ pai"
(defun cria-no (tabuleiro &optional (g 0) (h 0) (pai nil))
  (list tabuleiro g h pai)
)

(defun tabuleiro-inicial ()
  "Retorna um tabuleiro 5x6 (5 caixas na vertical por 6 caixas na horizontal)"
  '((
    ((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))
    ((0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
    ) (0 0)))

(defun tabuleiro-teste ()
  "Retorna um tabuleiro 5x6 (5 caixas na vertical por 6 caixas na horizontal)"
  '((
    ((1 0 0 0 0 0) (1 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0))
    ((1 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
    ) (0 0)))

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

;;Teste: (contar-ocorrencias-elemento (obter-estado-caixas (no-estado (tabuleiro-teste))) 0)
;;Resultado: 3
(defun contar-ocorrencias-elemento (lista elemento)
  (cond ((null lista) 0)
        ((equal elemento (car lista)) (+ 1 (contar-ocorrencias-elemento (cdr lista) elemento)))
        (t (contar-ocorrencias-elemento (cdr lista) elemento))))

;;Teste:(tabuleiro-preenchidop (no-estado (tabuleiro-teste)))
;;Resultado: NIL
(defun tabuleiro-preenchidop (estado)
  (let* ((arcos-hor (get-arcos-horizontais estado))
         (arcos-vert (get-arcos-verticais estado))
         (num-arcos-hor (length arcos-hor))
         (num-arcos-vert (length arcos-vert))
         (indice-a-usar (max num-arcos-hor num-arcos-vert)))
    (labels ((ver-tabuleiro (l c)
               (cond ((> l indice-a-usar) T)
                     (t (let ((valor-arco-hor (get-arco-na-posicao l c arcos-hor))
                              (valor-arco-vert (get-arco-na-posicao l c arcos-vert)))
                          (cond ((or (eq valor-arco-hor 0)(eq valor-arco-vert 0)) NIL)
                                ((= c indice-a-usar) (ver-tabuleiro (1+ l) 1))
                                (t (ver-tabuleiro l (1+ c)))))))))
      (ver-tabuleiro 1 1))))

;;;;;;;;;;;;;;;;;;
;;; Selectores ;;;
;;;;;;;;;;;;;;;;;;
(defun no-estado (no)
  (first no))

(defun no-caixas (no)
  (second no))

(defun get-arcos-horizontais (tabuleiro-estado)
  "Retorna os arcos horizontais de um tabuleiro"
  (first tabuleiro-estado))

(defun get-arcos-verticais (tabuleiro-estado)
  "Retorna os arcos verticais de um tabuleiro"
  (second tabuleiro-estado))

(defun get-arco-na-posicao (pos-lista-arcos pos-arco arcos)
  "Retorna o arco numa posicao passada por argumento de uma lista de arcos horizontais ou verticais"
  (nth (1- pos-arco) (nth (1- pos-lista-arcos) arcos)))

;;;;;;;;;;;;;;;;;;
;;; Operadores ;;;
;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;
;;; Heuristicas ;;;
;;;;;;;;;;;;;;;;:;;

;;primeira heuristica - h(x) = o(x) - c(x)
;;o(x) - numero de caixas necessarias para acabar o problema
;;c(x) - numero de caixas fechadas
(defun heuristica-base (estado num-caixas-a-fechar)
  (- num-caixas-a-fechar (contar-caixas-fechadas estado)))

;;segunda heuristica - h(x) = (w * (o(x) - c(x))) - (w2 * a(x)) - (w3 * b(x))
;;o(x) - numero de caixas necessarias para acabar o problema
;;c(x) - numero de caixas fechadas
;;a(x) - numero de caixas com 3 lados fechados
;;b(x) - numero de caixas com 2 lados fechados
;;w1, w2, w3 - fatores de ponderação
(defun heuristica-melhorada (estado num-caixas-a-fechar)
  (let ((estado-caixas (obter-estado-caixas estado)))
    (- (* (- num-caixas-a-fechar (contar-ocorrencias-elemento estado-caixas 4)) 10) (* (contar-ocorrencias-elemento estado-caixas 3) 6) (* (contar-ocorrencias-elemento estado-caixas 2) 3))))

(defun avaliacao (estado)
  (let ((caixas (obter-estado-caixas estado)))
    (+ (* 5 (contar-ocorrencias-elemento caixas 3)) (* -5 (contar-ocorrencias-elemento caixas 2)) (* 10 (second (no-caixas estado))) (* -10 (first (no-caixas estado))))))

(defun vencedor (caixas)
  (cond ((> (first caixas) (second caixas)) "Jogador 1")
        (t "Jogador 2")))

;;Teste: (contar-caixas-fechadas (no-estado (tabuleiro-teste)))
;;Resultado: 1
;Usando lista com lados preenchidos
(defun contar-caixas-fechadas1 (estado &optional (l 1) (i 1))
  (let* ((arcos-hor (get-arcos-horizontais estado))
         (arcos-vert (get-arcos-verticais estado))
         (num-total-arcos-hor (1- (length arcos-hor)))
         (num-total-indices-hor (length (car arcos-hor))))
    (labels ((iterar-tabuleiro (l i)
               (cond ((> l num-total-arcos-hor) 0)
                     (t (let ((caixa (estado-caixa arcos-hor arcos-vert l i)))
                          (cond ((and (= caixa 4) (= i num-total-indices-hor)) (+ 1 (iterar-tabuleiro (1+ l) 1)))
                                ((= caixa 4) (+ 1 (iterar-tabuleiro l (1+ i))))
                                ((= i num-total-indices-hor) (iterar-tabuleiro (1+ l) 1))
                                (t (iterar-tabuleiro l (1+ i)))))))))
      (iterar-tabuleiro l i))))

;;Usando predicado
(defun contar-caixas-fechadas (estado &optional (l 1) (i 1))
  (let* ((arcos-hor (get-arcos-horizontais estado))
         (arcos-vert (get-arcos-verticais estado))
         (num-total-arcos-hor (1- (length arcos-hor)))
         (num-total-indices-hor (length (car arcos-hor))))
    (labels ((iterar-tabuleiro (l i)
               (cond ((> l num-total-arcos-hor) 0)
                     (t (let ((caixa (caixa-fechadap arcos-hor arcos-vert l i)))
                          (cond ((and caixa (= i num-total-indices-hor)) (+ 1 (iterar-tabuleiro (1+ l) 1)))
                                (caixa (+ 1 (iterar-tabuleiro l (1+ i))))
                                ((= i num-total-indices-hor) (iterar-tabuleiro (1+ l) 1))
                                (t (iterar-tabuleiro l (1+ i)))))))))
      (iterar-tabuleiro l i))))

;;Teste: (contar-caixas-perto-fechar (no-estado (tabuleiro-teste)))
;;Resultado: 1
(defun contar-caixas-perto-fechar (estado &optional (l 1) (i 1))
  (let* ((arcos-hor (get-arcos-horizontais estado))
         (arcos-vert (get-arcos-verticais estado))
         (num-total-arcos-hor (1- (length arcos-hor)))
         (num-total-indices-hor (length (car arcos-hor))))
    (labels ((iterar-tabuleiro (l i)
               (novo-estado (funcall operador l i (no-estado no) jogador))
        (caixas-jogador-1 (first (no-caixas no)))
        (caixas-jogador-2 (contar-caixas-fechadas novo-estado)))tabuleiro (l i)
               (cond ((> l num-total-arcos-hor) 0)
                     (t (let ((caixa (estado-caixa arcos-hor arcos-vert l i)))
                          (cond ((and (= caixa 3) (= i num-total-indices-hor)) (+ 1 (iterar-tabuleiro (1+ l) 1)))
                                ((= caixa 3) (+ 1 (iterar-tabuleiro l (1+ i))))
                                ((= i num-total-indices-hor) (iterar-tabuleiro (1+ l) 1))
                                (t (iterar-tabuleiro l (1+ i)))))))))
      (iterar-tabuleiro l i))))

;;Teste: (obter-estado-caixas (no-estado (tabuleiro-teste)))
;;Resultado: (0 0 1 1 2 3 0 2 4), isto significa que existem 3 caixas sem lados preenchidos, 2 com 1 lado, 2 com 2 lados, 1 com 3 lados e 1 fechada.
(defun obter-estado-caixas (estado &optional (l 1) (i 1))
  (let* ((arcos-hor (get-arcos-horizontais estado))
         (arcos-vert (get-arcos-verticais estado))
         (num-total-arcos-hor (1- (length arcos-hor)))
         (num-total-indices-hor (length (car arcos-hor))))
    (labels ((iterar-tabuleiro (l i)
               (cond ((> l num-total-arcos-hor) NIL)
                     (t (let ((caixa (estado-caixa arcos-hor arcos-vert l i)))
                          (cond ((= i num-total-indices-hor) (cons caixa (iterar-tabuleiro (1+ l) 1)))
                                (t (cons caixa (iterar-tabuleiro l (1+ i))))))))))
      (iterar-tabuleiro l i))))

;;Teste: (caixa-fechadap (get-arcos-horizontais (no-estado (tabuleiro-teste))) (get-arcos-verticais (no-estado(tabuleiro-teste))) 3 3)
;;Resultado: T
(defun estado-caixa(arcos-hor arcos-vert l i)
  (let ((lado-1 (get-arco-na-posicao l i arcos-hor))
        (lado-2 (get-arco-na-posicao (1+ l) i arcos-hor))
        (lado-3 (get-arco-na-posicao i l arcos-vert))
        (lado-4 (get-arco-na-posicao (1+ i) l arcos-vert)))
    (cond ((and (null lado-1) (null lado-2) (null lado-3) (null lado-4)) 0)
          (t (+ lado-1 lado-2 lado-3 lado-4)))))

(defun caixa-fechadap(arcos-hor arcos-vert l i)
  (let ((lado-1 (get-arco-na-posicao l i arcos-hor))
        (lado-2 (get-arco-na-posicao (1+ l) i arcos-hor))
        (lado-3 (get-arco-na-posicao i l arcos-vert))
        (lado-4 (get-arco-na-posicao (1+ i) l arcos-vert)))
    (cond ((or (eq lado-1 0) (eq lado-2 0) (eq lado-3 0) (eq lado-4 0)) NIL)
          (t T))))

;;;;;;;;;;;;;;;;;;
;;; Sucessores ;;;
;;;;;;;;;;;;;;;;;;
(defun novo-sucessor (no l i operador jogador)
  (let* ((novo-estado (funcall operador l i (no-estado no) jogador))
        (caixas-jogador-1 (first (no-caixas no)))
        (caixas-jogador-2 (contar-caixas-fechadas novo-estado)))
    (list novo-estado (list caixas-jogador-1 caixas-jogador-2))))

(defun sucessores (no operadores jogador)
  (let* ((arcos-hor (get-arcos-horizontais (no-estado no)))
         (arcos-vert (get-arcos-verticais (no-estado no)))
         (l-maximo (max (length arcos-hor) (length arcos-vert)))
         (i-maximo (max (length (car arcos-hor)) (length (car arcos-vert)))))
    (labels ((iterar-tabuleiro (l i)
               (cond ((> l l-maximo) NIL)
                     ((> i i-maximo) (iterar-tabuleiro (1+ l) 1))
                     (t (append (mapcar (lambda (op) (novo-sucessor no l i op jogador)) operadores) (iterar-tabuleiro l (1+ i)))))))
      (iterar-tabuleiro 1 1))))
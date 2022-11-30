;;;; puzzle.lisp
;;;; Funcoes especificas ao dominio do jogo Dots and Boxes
;;;; Autor: Daniel Baptista, Rafael Silva


;;; Tabuleiro

(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
  '(
    ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
    ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
    ))

;;Seletores

(defun get-arcos-horizontais (tabuleiro)
  (first tabuleiro))

(defun get-arcos-verticais (tabuleiro)
  (second tabuleiro))

(defun get-arco-na-posicao (pos-lista-arcos pos-arco tabuleiro)
  (nth (1- pos-arco) (nth (1- pos-lista-arcos) tabuleiro)))

;;substituir

(defun substituir (indice lista &optional (x 1))
  (cond ((null lista) nil)
        ((= (1- indice) 0) (cons x (substituir (1- indice) (cdr lista) x)))
        (t (cons (car lista) (substituir (1- indice) (cdr lista) x)))))

(defun arco-na-posicao (pos-lista-arcos pos-arco lista-arcos &optional (x 1))
  (cond ((= (1- pos-lista-arcos) 0) (cons (substituir pos-arco (car lista-arcos) x) (cdr lista-arcos)))
        (t (cons (car lista-arcos) (arco-na-posicao (1- pos-lista-arcos) pos-arco (cdr lista-arcos) x)))))

;;Operadores

(defun arco-horizontal (pos-lista-arcos pos-arco tabuleiro &optional (x 1))
  (let* ((arcos-hor (car tabuleiro))
        (linhas (length arcos-hor))
        (colunas (length (car arcos-hor))))
    (cond ((or (> pos-lista-arcos linhas)(> pos-arco colunas)) NIL)
          ((= 1 (get-arco-na-posicao pos-lista-arcos pos-arco arcos-hor)) NIL)
          (t (cons (arco-na-posicao pos-lista-arcos pos-arco arcos-hor x) (cdr tabuleiro))))))

(defun arco-vertical (pos-lista-arcos pos-arco tabuleiro &optional (x 1))
  (let* ((arcos-ver (cadr tabuleiro))
        (linhas (length arcos-ver))
        (colunas (length (car arcos-ver))))
    (cond ((or (> pos-lista-arcos linhas)(> pos-arco colunas)) NIL)
          ((= 1 (get-arco-na-posicao pos-lista-arcos pos-arco arcos-ver)) NIL)
          (t (cons (car tabuleiro)(arco-na-posicao pos-lista-arcos pos-arco arcos-ver x))))))
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
  (nth (1- pos-arco) (nth (1- pos-hori) tabuleiro)))

;;substituir

(defun substituir (indice lista &optional (x 1))
  (cond ((null lista) nil)
        ((= (1- indice) 0) (cons x (substituir (1- indice) (cdr lista) x)))
        (t (cons (car lista) (substituir (1- indice) (cdr lista) x)))))

(defun arco-na-posicao (pos-lista-arcos pos-arco lista-arcos &optional (x 1))
  (cons (car lista-arcos) (cons (substituir pos-arco (nth pos-lista-arcos lista-arcos) x) (cdr lista-arcos))))

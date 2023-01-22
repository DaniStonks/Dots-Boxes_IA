;;;; procura.lisp
;;;; Funcoes dos metodos de procura
;;;; Autor: Daniel Baptista, Rafael Silva

;;;;;;;;;;;;;;;;;;;;;;;;
;; Funções auxiliares ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun filtrar-nos-filhos (nos)
  (reduce 'append (mapcar (lambda (no)
                            (cond((null (no-estado no)) NIL)
                                 (t (list no))))
                          nos)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Metodos de procura ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;Teste: (alfabeta (tabuleiro-teste) (operadores) 'sucessores 'avaliacao 2 2)
;;Resultado: 5
(defun alfabeta (no operadores sucessores avaliacao profundidade jogador)
  (labels ((maximizar (nos &optional (valor -10000000))
             (cond ((null nos) valor)
                   (t (let ((temp-valor (max valor (alfabeta (car nos) operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador)))))
                        (setf *alfa* (max *alfa* temp-valor))
                        (cond ((> *alfa* *beta*) temp-valor)
                              (t (maximizar (cdr nos) temp-valor)))))))
           (minimizar (nos &optional (valor 10000000))
             (cond ((null nos) valor)
                   (t (let ((temp-valor (min valor (alfabeta (car nos) operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador)))))
                        (setf *beta* (min *beta* temp-valor))
                        (cond ((< *beta* *alfa*) temp-valor)
                              (t (minimizar (cdr nos) temp-valor))))))))
    (cond ((= 0 profundidade) (avaliacao no))
          (t (let ((nos-filhos (filtrar-nos-filhos (sucessores no operadores jogador))))
               (cond ((= jogador 2) (maximizar nos-filhos))
                     (t (minimizar nos-filhos))))))))

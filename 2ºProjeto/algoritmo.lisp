;;;; procura.lisp
;;;; Funcoes dos metodos de procura
;;;; Autor: Daniel Baptista, Rafael Silva

;;;;;;;;;;;;;;
;; Closures ;;
;;;;;;;;;;;;;;
(let ((x 0)(alfa -99999) (beta 99999)(cortes-alfa 0)(cortes-beta 0)(nos-analisados 0))
  (defun set-alfa (valor)
    (setq alfa valor))
  (defun set-beta (valor)
    (setq beta valor))

  (defun get-alfa ()
    alfa)
  (defun get-beta ()
    beta)
  (defun get-nos-analisados ()
    nos-analisados)
  (defun get-cortes-alfa ()
    cortes-alfa)
  (defun get-cortes-beta ()
    cortes-beta)

  (defun inc-nos-analisados ()
    (setq nos-analisados (1+ nos-analisados)))
  (defun inc-cortes-alfa ()
    (setq cortes-alfa (1+ cortes-alfa)))
  (defun inc-cortes-beta ()
    (setq cortes-beta (1+ cortes-beta)))

  (defun reset-alfa-beta ()
    (set-alfa 0)
    (set-beta 0))
  (defun reset-estatisticas ()
    (setq cortes-alfa 0)
    (setq cortes-beta 0)
    (setq nos-analisados 0))
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;; Funções auxiliares ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun filtrar-nos-filhos (nos)
  (reduce 'append (mapcar (lambda (no)
                            (cond((null (no-tabuleiro no)) NIL)
                                 (t (list no))))
                          nos)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Metodos de procura ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;Teste: (alfabeta (tabuleiro-teste) (operadores) 'sucessores 'avaliacao 2 2)
;;Resultado: 5
(defun alfabeta2 (no operadores sucessores avaliacao profundidade jogador)
  (labels ((maximizar (nos &optional (valor -10000000))
             (cond ((null nos) valor)
                   (t (let ((temp-valor (max valor (alfabeta (car nos) operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador))))
                            (nos-analisados (inc-nos-analisados)))
                        (set-alfa (max (get-alfa) temp-valor))
                        (cond ((> (get-alfa) (get-beta)) (let ((cortes-beta (inc-cortes-beta)))
                                                           temp-valor))
                              (t (maximizar (cdr nos) temp-valor)))))))
           (minimizar (nos &optional (valor 10000000))
             (cond ((null nos) valor)
                   (t (let ((temp-valor (min valor (alfabeta (car nos) operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador))))
                            (nos-analisados (inc-nos-analisados)))
                        (set-beta (min (get-beta) temp-valor))
                        (cond ((< (get-beta) (get-alfa)) (let ((cortes-alfa (inc-cortes-alfa)))
                                                           temp-valor))
                              (t (minimizar (cdr nos) temp-valor))))))))
    (cond ((or (= 0 profundidade) (tabuleiro-preenchidop (no-tabuleiro no))) (avaliacao no))
          (t (let ((nos-filhos (filtrar-nos-filhos (sucessores no operadores jogador))))
               (cond ((= jogador 2) (maximizar nos-filhos))
                     (t (minimizar nos-filhos))))))))


;;Teste: (alfabeta (tabuleiro-teste) -99999 99999 (operadores) 'sucessores 'avaliacao 2 2)
;;Resultado: 5
(defun alfabeta (no alfa beta operadores sucessores avaliacao profundidade jogador)
  (labels ((maximizar (nos alf bet &optional (valor -10000000))
             (cond ((null nos) valor)
                   (t (let* ((temp-valor (max valor (alfabeta (car nos) alf bet operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador))))
                             (nos-analisados (inc-nos-analisados))
                             (temp-alfa (max temp-valor alfa)))
                        (cond ((>= temp-alfa beta) (let ((cortes-beta (inc-cortes-beta)))
                                                     beta))
                              (t (maximizar (cdr nos) temp-alfa bet temp-valor)))))))
           (minimizar (nos alf bet &optional (valor 10000000))
             (cond ((null nos) valor)
                   (t (let* ((temp-valor (min valor (alfabeta (car nos) alf bet operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador))))
                             (nos-analisados (inc-nos-analisados))
                             (temp-beta (min temp-valor beta)))
                        (cond ((<= temp-beta alfa) (let ((cortes-alfa (inc-cortes-alfa)))
                                                    temp-valor))
                              (t (minimizar (cdr nos) alf temp-beta temp-valor))))))))
    (cond ((or (= 0 profundidade) (tabuleiro-preenchidop (no-tabuleiro no))) (avaliacao no))
          (t (let ((nos-filhos (filtrar-nos-filhos (sucessores no operadores jogador))))
               (cond ((= jogador 2) (maximizar nos-filhos alfa beta))
                     (t (minimizar nos-filhos alfa beta))))))))

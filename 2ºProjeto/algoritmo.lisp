;;;; procura.lisp
;;;; Funcoes dos metodos de procura
;;;; Autor: Daniel Baptista, Rafael Silva

;;;;;;;;;;;;;;
;; Closures ;;
;;;;;;;;;;;;;;
(let ((cortes-alfa 0)(cortes-beta 0)(nos-analisados 0))
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

  (defun reset-estatisticas ()
    (setq cortes-alfa 0)
    (setq cortes-beta 0)
    (setq nos-analisados 0))
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;; Funções auxiliares ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun filtrar-nos-filhos (nos)
  "Função para remover os nós em que o tabuleiro seja NIL"
  (reduce 'append (mapcar (lambda (no)
                            (cond((null (no-tabuleiro no)) NIL)
                                 (t (list no))))
                          nos)))

;;;;;;;;;;;;;;
;; Alfabeta ;;
;;;;;;;;;;;;;;
;;Teste: (alfabeta (tabuleiro-teste) -99999 99999 (operadores) 'sucessores 'avaliacao 2 2)
;;Resultado: 5
(defun alfabeta (no alfa beta operadores sucessores avaliacao profundidade jogador)
  "Algoritmo de procura da melhor jogada possivel implementado com MiniMax com cortes Alfa-Beta"
  (labels ((maximizar (nos alf bet &optional (valor -10000000))
             (cond ((null nos) alf)
                   (t (let* ((temp-valor (max valor (alfabeta (car nos) alf bet operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador))))
                             (nos-analisados (inc-nos-analisados))
                             (temp-alfa (max temp-valor alf)))
                        (cond ((>= temp-alfa bet) (let ((cortes-beta (inc-cortes-beta)))
                                                     bet))
                              (t (maximizar (cdr nos) temp-alfa bet temp-valor)))))))
           (minimizar (nos alf bet &optional (valor 10000000))
             (cond ((null nos) bet)
                   (t (let* ((temp-valor (min valor (alfabeta (car nos) alf bet operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador))))
                             (nos-analisados (inc-nos-analisados))
                             (temp-beta (min temp-valor bet)))
                        (cond ((<= temp-beta alf) (let ((cortes-alfa (inc-cortes-alfa)))
                                                    alf))
                              (t (minimizar (cdr nos) alf temp-beta temp-valor))))))))
    (cond ((or (= 0 profundidade) (tabuleiro-preenchidop (no-tabuleiro no))) (avaliacao no))
          (t (let ((nos-filhos (filtrar-nos-filhos (sucessores no operadores jogador))))
               (cond ((= jogador 2) (maximizar nos-filhos alfa beta))
                     (t (minimizar nos-filhos alfa beta))))))))

;;;;       Programa do apoio ao jogo do galo
;;;;       Apos ter desenvolvido as funcoes pedidas no enunciado
;;;;       utilize a funcao fazer-uma-partida para jogar.

;;;;
;;;; Constantes:
;;;;
(defvar *jogador2* -1)
(defvar *jogador1* 1)
(defvar *jogada* NIL)


;;;;
;;;; Representacao do problema:
;;;;

.-.-.
| | |
. . .
. . .


;;;;
;;;; Selectores
;;;;

(defun linha (linha tabuleiro)
  (cond ((or (< linha 0) (>= linha (length tabuleiro))) NIL)
        (t (nth linha tabuleiro)))
) 

(defun coluna (coluna tabuleiro)
  (mapcar #'(lambda(linha) 
             (cond ((or (< coluna 0) (>= coluna (length linha))) NIL)
                   (t (nth coluna linha))                   
             ) 
           ) tabuleiro)
)

(defun diagonal (diagonal tabuleiro &optional (linha (1- (length tabuleiro))) &aux (dimensao (1- (length tabuleiro))))
  (cond ((or (< linha 0) (< diagonal 0) (> diagonal 1)) NIL)
        (t (append (diagonal diagonal tabuleiro (1- linha))
                   (list (celula linha (abs (- (* diagonal dimensao) linha)) tabuleiro))))
  )
)

(defun celula (linha coluna tabuleiro)
  (linha linha (coluna coluna tabuleiro))
)


;;;;
;;;; Funcoes auxiliares
;;;; 
(defun substituir-posicao (posicao lista &optional (valor 0) &aux (resto (cdr lista)))
  (cond ((null lista) NIL)
        ((< posicao 0) lista)
        ((= posicao 0) (cons valor resto))
        (t (cons (car lista) (substituir-posicao (1- posicao) resto valor)))
  )
)

(defun colocar-peca (linha coluna tabuleiro &optional (valor 0))
  (substituir-posicao linha tabuleiro (substituir-posicao coluna (linha linha tabuleiro) valor))
)

;;;; 
;;;; Entrada / saida de dados
;;;;

;;; Funcoes para imprimir um tabuleiro
;;; ----------------------------------
(defun imprime-tabuleiro (tabuleiro)
  (let ((numLinhas (1- (length tabuleiro))))
    (labels ((imprimir-tabuleiro (l)
               (cond ((> l numLinhas) (format t "~%"))
                     ((progn 
                       (format t "~A ~%" (linha l tabuleiro))
                       (imprimir-tabuleiro (1+ l)))))))
      (imprimir-tabuleiro 0))))
   
;;;;
;;;; Funcoes de jogo (humano e computador c/minimax)
;;;;

(defun jogar-humano (tabuleiro jogador)
  (progn 
    (imprime-tabuleiro tabuleiro)
    (let ((vencedor (tabuleiro-solucao (jogada-humano tabuleiro jogador))))
      (cond ((stringp vencedor) vencedor)
            ((tabuleiro-preenchidop vencedor) (format t "Empate"))
            (t (jogar-humano vencedor (trocar-peca jogador)))))))
         
(defun jogada-humano (tabuleiro jogador)
  (let ((linha (progn
                 (format t "Qual a linha onde colocar a peça? ~%")
                 (read)))
        (coluna (progn
                 (format t "Qual a coluna onde colocar a peça? ~%")
                 (read))))
    (colocar-peca linha coluna tabuleiro jogador)))
        


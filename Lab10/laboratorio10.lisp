;;;;       Programa do apoio ao jogo do galo
;;;;       Apos ter desenvolvido as funcoes pedidas no enunciado
;;;;       utilize a funcao fazer-uma-partida para jogar.

;;;;
;;;; Constantes:
;;;;
(defvar *jogador2* -1)
(defvar *jogador1* 1)


;;;;
;;;; Representacao do problema:
;;;;

;;; Criacao de tabuleiro
;;; ----------------------------------
(defun tabuleiro-inicial (&optional stream)
  "Permite criar o tabuleiro inicial do jogo."
  (cond ((null stream) '((0 0 0)
                         (0 0 0)
                         (0 0 0)))
        (t (read stream))))

(defun tabuleiro-teste (&optional (jogador *jogador1*) &aux (jogador-oposto (trocar-peca jogador)))
  (list (list jogador 0 jogador-oposto)
        (list 0 0 jogador-oposto)
        (list jogador 0 0)
  )
)


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
  (let ((numLinhas (length tabuleiro)))
    (labels ((imprimir-tabuleiro (l)
               (cond ((< l 0) (format t "~%"))
                     ((progn 
                       (format t "~A ~%" (linha l tabuleiro))
                       (imprimir-tabuleiro (1- l)))))))
      (imprimir-tabuleiro (1- numLinhas)))))

;;;;
;;;; Funcoes para o jogo
;;; ----------------------------------
(defun tabuleiro-preenchidop (tabuleiro)
  (let* ((numLinhas (length tabuleiro))
         (numColunas (length (car tabuleiro))))
    (labels ((ver-tabuleiro (l c)
               (cond ((> l num-total-arcos-hor) T)
                     (t (let ((valor-pos (celula l c tabuleiro)))
                          (cond ((= valor-pos 0) F)
                                ((= i numColunas) (iterar-tabuleiro (1+ l) 0))
                                (t (iterar-tabuleiro l (1+ i)))))))))
      (ver-tabuleiro (0 0))))

(defun tabuleiro-solucao (tabuleiro)
  (let* ((numLinhas (length tabuleiro))
         (numColunas (length (car tabuleiro))))
    (labels ((ver-tabuleiro (l c)
               (cond ((> l num-total-arcos-hor) T)
                     (t (let ((valor-pos (celula l c tabuleiro)))
                          (cond ((= valor-pos 0) F)
                                ((= i numColunas) (iterar-tabuleiro (1+ l) 0))
                                (t (iterar-tabuleiro l (1+ i)))))))))
      (ver-tabuleiro (0 0))))

;;;;
;;;; Funcoes de jogo (humano e computador c/minimax)
;;;;

(defun trocar-peca (peca)
  "Troca a peca de um jogador para a peca de outro jogador."
  (- 0 peca)
)

(defun jogar-humano (tabuleiro jogador)
  (let ((vencedor (tabuleiro-solucao (jogada-humano tabuleiro jogador))))
    (cond (vencedor vencedor)
          ((tabuleiro-preenchidop) (format t "Empate"))
          (jogar-humano tabuleiro (trocar-peca jogador)))))
         
(defun jogada-humano (tabuleiro jogador)
  (let ((linha (progn
                 (format t "Qual a linha onde colocar a peça? ~%")
                 (read)))
        (coluna (progn
                 (format t "Qual a coluna onde colocar a peça? ~%")
                 (read))))
    (colocar-peca (linha coluna tabuleiro jogador))))


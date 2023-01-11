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
  (let ((numLinhas (1- (length tabuleiro))))
    (labels ((imprimir-tabuleiro (l)
               (cond ((> l numLinhas) (format t "~%"))
                     ((progn 
                       (format t "~A ~%" (linha l tabuleiro))
                       (imprimir-tabuleiro (1+ l)))))))
      (imprimir-tabuleiro 0))))

;;;;
;;;; Funcoes para o jogo
;;; ----------------------------------
(defun tabuleiro-preenchidop (tabuleiro)
  (let* ((numLinhas (1- (length tabuleiro)))
         (numColunas (1- (length (car tabuleiro)))))
    (labels ((ver-tabuleiro (l c)
               (cond ((> l numLinhas) T)
                     (t (let ((valor-pos (celula l c tabuleiro)))
                          (cond ((= valor-pos 0) NIL)
                                ((= c numColunas) (ver-tabuleiro (1+ l) 0))
                                (t (ver-tabuleiro l (1+ c)))))))))
      (ver-tabuleiro 0 0))))

(defun tabuleiro-solucao (tabuleiro)
  (let* ((dimensao (1- (length tabuleiro))))
    (labels ((ver-tabuleiro (l)
               (cond ((> l dimensao) tabuleiro)
                     (t (let ((valor-linha (linha l tabuleiro))
                              (valor-coluna (coluna l tabuleiro))
                              (valor-diagonal (diagonal l tabuleiro)))
                          (cond ((= (apply '+ valor-linha) 3) "Jogador 1")
                                ((= (apply '+ valor-linha) -3) "Jogador 2")
                                ((= (apply '+ valor-coluna) 3) "Jogador 1")
                                ((= (apply '+ valor-coluna) -3) "Jogador 2")
                                ((= (apply '+ valor-diagonal) 3) "Jogador 1")
                                ((= (apply '+ valor-diagonal) -3) "Jogador 2")
                                (t (ver-tabuleiro (1+ l)))))))))
      (ver-tabuleiro 0))))

(defun heuristica (tabuleiro jogador)
  (cond ((and (tabuleiro-solucao tabuleiro) (= jogador 1)) 100)
        
;;;;
;;;; Funcoes de jogo (humano e computador c/minimax)
;;;;

(defun trocar-peca (peca)
  "Troca a peca de um jogador para a peca de outro jogador."
  (- 0 peca)
)

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

(defun jogar-computador (tabuleiro jogador)
  (progn 
    (imprime-tabuleiro tabuleiro)
    (let ((vencedor (tabuleiro-solucao (jogada-humano tabuleiro jogador))))
      (cond ((stringp vencedor) vencedor)
            ((tabuleiro-preenchidop vencedor) (format t "Empate"))
            (t (jogar-computador vencedor (trocar-peca jogador)))))))

(defun jogada-computador (tabuleiro jogador)
  (let ((linha (progn
                 (format t "Qual a linha onde colocar a peça? ~%")
                 (read)))
        (coluna (progn
                 (format t "Qual a coluna onde colocar a peça? ~%")
                 (read))))
    (colocar-peca linha coluna tabuleiro jogador)))

;(max val (minimax 
(defun minimax (no profundidade peca)
  (cond ((= 0 profundidade) (heuristica no))
        (t (let ((nos-filhos (
        ((= 1 peca)
        


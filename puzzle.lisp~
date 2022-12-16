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
  '(
    ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
    ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
    ) 0 0 NIL)

;;Seletores
(defun no-estado (no)
  (first no))

(defun get-arcos-horizontais (tabuleiro)
  "Retorna os arcos horizontais de um tabuleiro"
  (first tabuleiro))

(defun get-arcos-verticais (tabuleiro)
  "Retorna os arcos verticais de um tabuleiro"
  (second tabuleiro))

(defun get-arco-na-posicao (pos-lista-arcos pos-arco tabuleiro)
  "Retorna o arco numa posicao passada por argumento de uma lista de arcos horizontais ou verticais"
  (nth (1- pos-arco) (nth (1- pos-lista-arcos) tabuleiro)))

(defun no-profundidade (no)
  (second no))

(defun no-heuristica (no)
  (third no))

(defun no-pai (no)
  (fourth no)

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
 (list 'vazar-a 'vazar-b 'encher-a 'encher-b 'transferir-a-b 'transferir-b-a))

(defun arco-horizontal (pos-lista-arcos pos-arco tabuleiro &optional (x 1))
  "Coloca um arco horizontal num tabuleiro, na posicao passada por argumento"
  (let* ((arcos-hor (car tabuleiro))
        (linhas (length arcos-hor))
        (colunas (length (car arcos-hor))))
    (cond ((or (> pos-lista-arcos linhas)(> pos-arco colunas)) NIL)
          ((= 1 (get-arco-na-posicao pos-lista-arcos pos-arco arcos-hor)) NIL)
          (t (cons (arco-na-posicao pos-lista-arcos pos-arco arcos-hor x) (cdr tabuleiro))))))

(defun arco-vertical (pos-lista-arcos pos-arco tabuleiro &optional (x 1))
  "Coloca um arco vertical num tabuleiro, na posicao passada por argumento"
  (let* ((arcos-ver (cadr tabuleiro))
        (linhas (length arcos-ver))
        (colunas (length (car arcos-ver))))
    (cond ((or (> pos-lista-arcos linhas)(> pos-arco colunas)) NIL)
          ((= 1 (get-arco-na-posicao pos-lista-arcos pos-arco arcos-ver)) NIL)
          (t (cons (car tabuleiro)(arco-na-posicao pos-lista-arcos pos-arco arcos-ver x))))))



(defun no-solucaop (no)
  (cond ((= (vasilha-a-conteudo no) 1) T)
        ((= (vasilha-b-conteudo no) 1) T)
        (t NIL)))

(defun heuristica (estado)
  (cond ((or (= 1 (first estado))(= 1 (second estado))) 0)
        ((and (= (first estado) (second estado))(/= 1 (first estado))) 1)
        (t 2))) 




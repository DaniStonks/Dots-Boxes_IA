;;;; projeto.lisp
;;;; Funcoes de interacao com o utilizador e de escrita e leitura de ficheiros
;;;; Autor: Daniel Baptista, Rafael Silva

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inicializacao do programa ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iniciar ()
  "Permite iniciar o programa, fazendo a leitura do teclado do estado inicial, do algoritmo, a heuristica e a profundidade se utilizadas"
  (let* ((carregamentoFicheiros (carregar-ficheiros (ler-diretoria)))
         (tabuleiro (tabuleiro-inicial))
         (tempo-computador (ler-tempo))
         (no-solucao (cond
                      ((equal algoritmo 'bfs) (funcall algoritmo no 'no-solucaop 'sucessores (operadores) num-solucao *abertos* *fechados*))
                      ((equal algoritmo 'dfs) (funcall algoritmo no 'no-solucaop 'sucessores (operadores) profundidade num-solucao *abertos* *fechados*))
                      ((equal algoritmo 'a*) (funcall algoritmo no 'no-solucaop 'sucessores (operadores) heuristica num-solucao *abertos* *fechados*))))
         (tempo-execucao (obter-tempo-execucao-em-segundos tempo-execucao-inicial (get-internal-real-time))))
    (mostrar-solucao no-solucao tempo-execucao)
    (escrever-no-log no-solucao algoritmo heuristica tempo-execucao diretoria)))

(defun carregar-ficheiros (diretoria)
  (progn
    (load (concatenate 'string diretoria "\\puzzle.lisp"))
    (load (concatenate 'string diretoria "\\algoritmo.lisp"))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Fun��es auxiliares ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;arco-horizontal (pos-lista-arcos pos-arco tabuleiro-estado &optional (x 1))
;;Teste: (colocar-arco (3 1 'ARCO-HORIZONTAL) (tabuleiro-teste))
;;Resultado: (estado novo)
(defun colocar-arco (jogada estado)
  (let* ((novo-estado (funcall (third jogada) (first jogada) (second jogada) (no-estado estado)))
         (novas-caixas (cond ((/= (+ (first (no-caixas estado)) (second (no-caixas estado))) (contar-caixas-n-lados novo-estado 4)) (list (1+ (first (no-caixas estado))) (second (no-caixas estado))))
                             (t (no-caixas estado)))))
    (list novo-estado novas-caixas)))

(defun trocar-jogador (jogador)
  (cond ((= jogador 1) 2)
        (t 1)))

(defun obter-tempo-em-segundos (tempo-inicial tempo-final)
  (float (/ (- tempo-final tempo-inicial) 1000)))

;;;;;;;;;;;;;;;;;;;;;
;; Fun��es de jogo ;;
;;;;;;;;;;;;;;;;;;;;;

;(jogo (tabuleiro-teste) 1)
(defun jogo (estado jogador)
  (progn
    (imprime-tabuleiro estado)
    (let ((novo-estado (cond ((= jogador 1) (jogar-humano estado))
                             (t (jogar estado 2)))))
      (cond ((tabuleiro-preenchidop (no-estado novo-estado)) (vencedor))
            (t (cond ((jogada-caixa-fechadap estado (no-estado novo-estado)) (jogo novo-estado jogador))
                     (t (jogo novo-estado (trocar-jogador jogador)))))))))

(defun jogar-humano (estado)
  (let ((jogada (ler-jogada)))
    (colocar-arco jogada estado)))
        
(defun jogar (estado &optional (profundidade 0) (tempo 0)) ;;apenas utilizada pelo computador, nunca pelo humano
  "Permite iniciar o programa, fazendo a leitura do teclado do estado inicial, do algoritmo, a heuristica e a profundidade se utilizadas"
  (let* ((melhor-jogada-aval (minimax estado (operadores) 'sucessores 'avaliacao profundidade 2))
         (novo-estado (selecionar-jogada-avaliacao estado 'avaliacao melhor-jogada-aval)))
    novo-estado))
         

;;;;;;;;;;;;;
;; Leitura ;;
;;;;;;;;;;;;;

(defun ler-diretoria()
"Permite fazer a leitura da diretoria do programa"
    (progn
      (format t "Qual a diretoria em que o programa reside?~%")
      (read-line)
      ))

(defun ler-tempo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Tempo limite por jogada para o computador(em segundos)? ~%")
    (let ((resposta (read)))
        (cond ((< resposta 1) 1000)
              ((> resposta 20) 20000)
              (t (* resposta 1000))))))

(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
      (format t "Qual a profundidade limite? ~%")
      (read)))

(defun ler-jogada ()
  (let* ((tipo-arco (ler-tipo-arco))
         (linha (ler-coordenada "linha"))
         (coluna (ler-coordenada "coluna")))
    (list linha coluna tipo-arco))) 

(defun ler-tipo-arco ()
  (progn
      (format t "Qual o tipo de arco a inserir? ~%")
      (format t "1 - Arco horizontal ~%")
      (format t "2 - Arco vertical ~%")
      (let ((resposta (read)))
        (cond ((= resposta 1) 'arco-horizontal)
              (t 'arco-vertical)))))

(defun ler-coordenada (eixo)
  (progn
      (format t "Em que ~A inserir o arco? ~%" eixo)
      (read)))

;;;;;;;;;;;;;
;; Escrita ;;
;;;;;;;;;;;;;

;;(imprime-tabuleiro (tabuleiro-inicial))
(defun imprime-tabuleiro (tabuleiro)
  (progn
    (format t "~A ~%" (get-arcos-horizontais (no-estado tabuleiro)))
    (format t "~A ~%" (get-arcos-verticais (no-estado tabuleiro)))))

;;o programa dever� escrever num ficheiro log.dat e no ecr� qual a jogada realizada, o novo estado, o n�mero de n�s analisados, o n�mero de cortes efetuados (de cada tipo) e o tempo gasto.
(defun imprimir-jogada (diretoria jogada num-nos num-cortes tempo)
  "Permite escrever no final do ficheiro log.dat as seguintes informa��es do problema, o estado inicial, a solu��o encontrada, o n�mero de n�s gerados e o n�mero de n�s expandidos"
  (with-open-file (stream (concatenate 'string diretoria "\\log.dat")
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (let ((nos-gerados (+ (length *abertos*)(length *fechados*)))
          (nos-expandidos (length *fechados*)))
      (progn
        (format stream "Algoritmo utilizado - ~A, ~@[~A~] ~%" algoritmo heuristica)
        (format stream "Solu��o encontrada: ~A ~%" (no-estado no-solucao))
        (format stream "Estado inicial: ~A ~%" (estado-no-inicial no-solucao))
        (format stream "N�mero de n�s gerados: ~A | N�mero de n�s expandidos: ~A ~%" nos-gerados nos-expandidos)
        (format stream "Penetr�ncia: ~A | Factor de ramifica��o medio: ~A | Tempo de execu��o: ~A ~%" (penetrancia (no-profundidade no-solucao) nos-gerados) (bisseccao 'f-fator-ramificacao 0 10 no-solucao) tempo-execucao)
        (format stream "Caminho ~%")
        (escreve-lista-nos no-solucao stream)
        (format stream "~% ~% ~%")))))

;;o programa dever� escrever num ficheiro log.dat e no ecr� qual a jogada realizada, o novo estado, o n�mero de n�s analisados, o n�mero de cortes efetuados (de cada tipo) e o tempo gasto.
(defun escrever-no-log (diretoria jogada num-nos num-cortes tempo)
  "Permite escrever no final do ficheiro log.dat as seguintes informa��es do problema, o estado inicial, a solu��o encontrada, o n�mero de n�s gerados e o n�mero de n�s expandidos"
  (with-open-file (stream (concatenate 'string diretoria "\\log.dat")
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (let ((nos-gerados (+ (length *abertos*)(length *fechados*)))
          (nos-expandidos (length *fechados*)))
      (progn
        (format stream "Algoritmo utilizado - ~A, ~@[~A~] ~%" algoritmo heuristica)
        (format stream "Solu��o encontrada: ~A ~%" (no-estado no-solucao))
        (format stream "Estado inicial: ~A ~%" (estado-no-inicial no-solucao))
        (format stream "N�mero de n�s gerados: ~A | N�mero de n�s expandidos: ~A ~%" nos-gerados nos-expandidos)
        (format stream "Penetr�ncia: ~A | Factor de ramifica��o medio: ~A | Tempo de execu��o: ~A ~%" (penetrancia (no-profundidade no-solucao) nos-gerados) (bisseccao 'f-fator-ramificacao 0 10 no-solucao) tempo-execucao)
        (format stream "Caminho ~%")
        (escreve-lista-nos no-solucao stream)
        (format stream "~% ~% ~%")))))
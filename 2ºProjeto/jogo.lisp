;;;; projeto.lisp
;;;; Funcoes de interacao com o utilizador e de escrita e leitura de ficheiros
;;;; Autor: Daniel Baptista, Rafael Silva

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inicializacao do programa ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iniciar ()
  "Permite iniciar o programa, fazendo a leitura do teclado do estado inicial, do algoritmo, a heuristica e a profundidade se utilizadas"
  (let* ((diretoria (ler-diretoria))
         (carregamentoFicheiros (carregar-ficheiros diretoria))
         (estado-inicial (tabuleiro-inicial))
         (modo-jogo (ler-modo-jogo))
         (profundidade-limite (ler-profundidade)))
    (cond ((equal modo-jogo 'jogo-pessoa-vs-pc) (funcall modo-jogo diretoria estado-inicial (ler-jogador-inicial) profundidade-limite))
          (t jogo-pc-vs-pc))))

(defun carregar-ficheiros (diretoria)
  (progn
    (load (concatenate 'string diretoria "\\puzzle.lisp"))
    (load (concatenate 'string diretoria "\\algoritmo.lisp"))
    (format t "~%")))

;;;;;;;;;;;;;;;;;;;;;
;; Funções de jogo ;;
;;;;;;;;;;;;;;;;;;;;;

(defun jogo-pessoa-vs-pc (diretoria estado jogador profundidade)
  (progn
    (imprime-tabuleiro estado)
    (let* ((nova-jogada (cond ((= jogador 1) (jogar-humano estado))
                              (t (jogar estado profundidade))))
           (novo-estado (second nova-jogada)))
      (cond ((not (jogada-validap novo-estado)) (progn
                                                  (format t "Jogada invalida~%")
                                                  (jogo-pessoa-vs-pc novo-estado jogador profundidade)))
            (t (let* ((escrever-ecra (imprimir-jogada diretoria nova-jogada jogador))
                      (escrever-ficheiro (escrever-no-log diretoria nova-jogada jogador)))
                 (cond ((tabuleiro-preenchidop (no-tabuleiro novo-estado)) (vencedor (no-caixas novo-estado)))
                       ((jogada-caixa-fechadap estado (no-tabuleiro novo-estado)) (jogo-pessoa-vs-pc diretoria novo-estado jogador profundidade))
                       (t (jogo-pessoa-vs-pc diretoria novo-estado (trocar-jogador jogador) profundidade)))))))))
;escrever-no-log (diretoria jogada jogador)
;imprimir-jogada (diretoria jogada jogador)
(defun jogo-pc-vs-pc ()
  "Não implementado")

(defun jogar-humano (estado)
  (let ((jogada (ler-jogada)))
    (colocar-arco jogada estado)))
        
(defun jogar (estado &optional (profundidade 0)) ;;apenas utilizada pelo computador, nunca pelo humano
  "Irá encontrar a melhor jogada possivel para o Computador e devolve o novo estado e a jogada efetuada"
  (let* ((melhor-jogada (obter-melhor-jogada estado profundidade))
         (jogada-efetuada (obter-jogada-atraves-estado-final estado melhor-jogada 2)))
    (list jogada-efetuada melhor-jogada)))

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
"Permite fazer a leitura da profundidade limite para o algoritmo alfabeta"
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

(defun ler-jogador-inicial ()
  (progn
    (format t "Qual o jogador a jogar primeiro? ~%")
    (format t "1 - Pessoa ~%")
    (format t "2 - Computador ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) 1)
            (t 2)))))

(defun ler-modo-jogo ()
  (progn
    (format t "Qual o modo de jogo? ~%")
    (format t "1 - Pessoa vs Computador ~%")
    (format t "2 - Computador vs Computador (Não implementado) ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) 'jogo-pessoa-vs-pc)
            (t 'jogo-pc-vs-pc)))))

;;;;;;;;;;;;;
;; Escrita ;;
;;;;;;;;;;;;;

;;(imprime-tabuleiro (tabuleiro-inicial))
(defun imprime-tabuleiro (tabuleiro)
  (progn
    (format t "~A ~%" (get-arcos-horizontais (no-tabuleiro tabuleiro)))
    (format t "~A ~%~%" (get-arcos-verticais (no-tabuleiro tabuleiro)))))

(defun imprimir-jogada (diretoria jogada jogador)
  (let* ((jogada-realizada (car jogada))
        (jogada-estado (cdr jogada)))
    (progn
      (format t "Linha - ~A; Coluna - ~A; Operação - ~A ~%" (first jogada-realizada) (second jogada-realizada) (third jogada-realizada))
      (format t "Estado atual: ~A ~%" (no-tabuleiro jogada-estado))
      (cond ((= jogador 2)
             (format t "Número de nós analisados: ~A ~%" (get-nos-analisados))
             (format t "Número de cortes alfa: ~A ~%" (get-cortes-alfa))
             (format t "Número de cortes beta: ~A ~%" (get-cortes-beta))))
      (format t "~%"))))
        

(defun escrever-no-log (diretoria jogada jogador)
  "Permite escrever no final do ficheiro log.dat as seguintes informações do problema, a jogada realizada, o estado atual, o número de nós analisados e o número de cortes(de cada tipo)"
  (with-open-file (stream (concatenate 'string diretoria "\\log.dat")
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (let* ((jogada-realizada (car jogada))
          (jogada-estado (cdr jogada)))
      (progn
        (cond ((= jogador 1) (format stream "Jogador 1")
               (t "Jogador 2")))
        (format stream "Linha - ~A; Coluna - ~A; Operação - ~A ~%" (first jogada-realizada) (second jogada-realizada) (third jogada-realizada))
        (format stream "Estado atual: ~A ~%" (no-tabuleiro jogada-estado))
        (cond ((= jogador 2)
               (format stream "Número de nós analisados: ~A ~%" (get-nos-analisados))
               (format stream "Número de cortes alfa: ~A ~%" (get-cortes-alfa))
               (format stream "Número de cortes beta: ~A ~%" (get-cortes-beta))))
        (format stream "~%~%")))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Funções auxiliares ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun colocar-arco (jogada estado)
  (let* ((novo-estado (funcall (third jogada) (first jogada) (second jogada) (no-tabuleiro estado)))
         (novas-caixas (let ((num-caixas (- (contar-caixas-n-lados novo-estado 4) (first (no-caixas estado)) (second (no-caixas estado)))))
                         (cond ((/= num-caixas 0) (list (+ num-caixas (first (no-caixas estado))) (second (no-caixas estado))))
                               (t (no-caixas estado))))))
    (list jogada (list novo-estado novas-caixas))))

(defun jogada-validap (jogada-estado)
  (cond ((null (no-tabuleiro jogada-estado)) NIL)
        (t T)))

(defun trocar-jogador (jogador)
  (cond ((= jogador 1) 2)
        (t 1)))

(defun obter-melhor-jogada (estado &optional (profundidade 0))
  (let* ((sucessores (filtrar-nos-filhos (sucessores estado (operadores) 2)))
         (estatisticas (reset-estatisticas))
         (alfabeta-lista (mapcar (lambda (filho)
                                   (reset-alfa-beta)
                                   (alfabeta filho (operadores) 'sucessores 'avaliacao 1 1))
                                 sucessores))
         (melhor-jogada-indice (obter-indice-elemento alfabeta-lista (apply 'max alfabeta-lista))))
    (nth melhor-jogada-indice sucessores)))

(defun obter-indice-elemento(lista elemento)
  (cond ((null lista) NIL)
        ((eq (car lista) elemento) 0)
        (t (1+ (obter-indice-elemento (cdr lista) elemento)))))

(defun obter-tempo-em-segundos (tempo-inicial tempo-final)
  (float (/ (- tempo-final tempo-inicial) 1000)))
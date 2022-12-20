;;;; projeto.lisp
;;;; Funcoes de interacao com o utilizador e de escrita e leitura de ficheiros
;;;; Autor: Daniel Baptista, Rafael Silva

(load "C:\\Users\\202001990\\Documents\\GitHub\\Dots-Boxes_IA\\puzzle.lisp")
(load "C:\\Users\\202001990\\Documents\\GitHub\\Dots-Boxes_IA\\procura.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inicializacao do programa ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iniciar ()
  "Permite iniciar o programa, fazendo a leitura do teclado do estado inicial, do algoritmo, a heuristica e a profundidade se utilizadas"
  (setf *abertos* nil)
  (setf *fechados* nil)
  (let* ((problema (ler-problema))
         (no (cria-no (car problema)))
         (num-solucao (second problema))
         (algoritmo (ler-algoritmo))
         (heuristica (cond ((eql algoritmo 'a*) (ler-heuristica)) (t NIL)))
         (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999)))
         (tempo-execucao (get-internal-run-time))
         (no-solucao (cond
                      ((equal algoritmo 'bfs) (funcall algoritmo no 'no-solucaop 'sucessores (operadores) num-solucao *abertos* *fechados*))
                      ((equal algoritmo 'dfs) (funcall algoritmo no 'no-solucaop 'sucessores (operadores) profundidade num-solucao *abertos* *fechados*))
                      ((equal algoritmo 'a*) (funcall algoritmo no 'no-solucaop 'sucessores (operadores) heuristica num-solucao *abertos* *fechados*)))))
    (progn 
      (mostrar-solucao no-solucao (- (get-internal-run-time) tempo-execucao))
      (escrever-no-log no-solucao algoritmo))))

;;;;;;;;;;;;;
;; Leitura ;;
;;;;;;;;;;;;;
(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura ~%")
    (format t "2- Procura na profundidade ~%")
    (format t "3- Algoritmo A* ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) 'bfs)
            ((= resposta 2) 'dfs)
            (T 'a*)))
    )
  )

(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
      (format t "Qual a profundidade limite? ~%")
      (read)
      ))

(defun ler-heuristica()
"Permite fazer a leitura da heuristica a usar para o algoritmo a*."
    (progn
      (format t "Qual a heuristica a usar? ~%")
      (format t "1 - Numero de caixas fechadas ~%")
      (format t "2 - Numero de caixas fechadas e numero de caixas perto de fechar ~%")
      (let ((resposta (read)))
        (cond ((= resposta 1) 'heuristica-base)
              ((= resposta 2) 'heuristica-melhorada)
              (t 'heuristica-melhorada)))))

(defun ler-problema ()
  (let ((num-problema (progn
                        (format t "Qual o problema a resolver? ")
                        (read))))
    (with-open-file (file "C:\\Users\\202001990\\Documents\\GitHub\\Dots-Boxes_IA\\Problemas\\problemas.dat" :direction :input)
      (let ((line-number 0))
        (loop for line = (read file nil)
              while line do
                (incf line-number)
                (when (= line-number num-problema)
                  (return (list line (ler-solucao num-problema)))))))))

(defun ler-solucao (num-problema)
  (with-open-file (file "C:\\Users\\202001990\\Documents\\GitHub\\Dots-Boxes_IA\\Problemas\\solucoes.dat" :direction :input)
      (let ((line-number 0))
        (loop for line = (read file nil)
              while line do
                (incf line-number)
                (when (= line-number num-problema)
                  (return line))))))


;;;;;;;;;;;;;
;; Escrita ;;
;;;;;;;;;;;;;
(defun escrever-no-log (no-solucao algoritmo)
  "Permite escrever no final do ficheiro log.dat as seguintes informações do problema, o estado inicial, a solução encontrada, o número de nós gerados e o número de nós expandidos"
  (with-open-file (stream "C:\\Users\\202001990\\Documents\\GitHub\\Dots-Boxes_IA\\Problemas\\log.dat"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (progn
      (format stream "Algoritmo utilizado - ~A ~%" algoritmo)
      (format stream "Estado inicial: ~A | Solução encontrada: ~A | Número de nós gerados: ~A | Número de nós expandidos: ~A ~% ~%" (estado-no-inicial no-solucao) (no-estado no-solucao) (+ (length *abertos*)(length *fechados*)) (length *fechados*)))))

(defun escreve-lista-nos (lista)
  "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
  (cond
   ((null lista) nil)
   (T (progn 
        (format t "Estado: ~A | Profundidade: ~A | Heuristica: ~A | Custo: ~A ~%" (no-estado lista) (no-profundidade lista) (no-heuristica lista) (no-custo lista)) 
        (escreve-lista-nos (no-pai lista))))))

(defun mostrar-solucao (no-solucao tempo-execucao)
  (progn
    (escreve-lista-nos no-solucao)
    (format t "Numero de nós gerados: ~A | Numero de nós expandidos: ~A | Penetrância: ~A | Tempo de execução: ~A segundos" (+ (length *abertos*)(length *fechados*)) (length *fechados*) (penetrancia (no-profundidade no-solucao) (+ (length *abertos*) (length *fechados*))) tempo-execucao)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Analise de resultados ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun penetrancia (comprimento-objetivo num-nos-gerados)
  (/ comprimento-objetivo num-nos-gerados))

(defun bisseccao ()
  )
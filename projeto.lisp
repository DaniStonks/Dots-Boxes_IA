;;;; projeto.lisp
;;;; Funcoes de interacao com o utilizador e de escrita e leitura de ficheiros
;;;; Autor: Daniel Baptista, Rafael Silva

(load "C:\\Users\\Daniel\\Desktop\\Coisas do ips\\3∫Ano\\1∫Semestre\\IA\\Proj\\Dots-Boxes_IA\\puzzle.lisp")
(load "C:\\Users\\Daniel\\Desktop\\Coisas do ips\\3∫Ano\\1∫Semestre\\IA\\Proj\\Dots-Boxes_IA\\procura.lisp")
(setf *abertos* nil)
(setf *fechados* nil)

;;; Inicializacao do programa
;; iniciar
(defun iniciar ()
"Permite iniciar o programa, fazendo a leitura do teclado do estado inicial e do algoritmo a utilizar para procurar a solu√ß√£o (neste caso a procura na profundidade ou na largura)"
  (let* ((problema (ler-problema))
         (no (cria-no (car problema)))
         (num-solucao (second problema))
         (algoritmo (ler-algoritmo))
         (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999))) )
	(cond
		((equal algoritmo 'bfs) (mostrar-solucao (funcall algoritmo no 'no-solucaop 'sucessores (operadores) num-solucao *abertos* *fechados*) *abertos* *fechados*))
		((equal algoritmo 'dfs) (mostrar-solucao (funcall algoritmo no 'no-solucaop 'sucessores (operadores) profundidade num-solucao *abertos* *fechados*) *abertos* *fechados*))
                ((equal algoritmo 'a*) (mostrar-solucao (funcall algoritmo no 'no-solucaop 'sucessores (operadores) 'heuristica num-solucao *abertos* *fechados*) *abertos* *fechados*))
	)
  )
)

;; ler-algoritmo
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

;; ler-profundidade
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
      (format t "Qual a profundidade limite? ~%")
      (read)
      ))

(defun ler-problema ()
  (let ((num-problema (progn
                        (format t "Qual o problema a resolver? ")
                        (read))))
    (with-open-file (file "C:\\Users\\Daniel\\Desktop\\Coisas do ips\\3∫Ano\\1∫Semestre\\IA\\Proj\\Dots-Boxes_IA\\Problemas\\problemas.dat" :direction :input)
      (let ((line-number 0))
        (loop for line = (read file nil)
              while line do
                (incf line-number)
                (when (= line-number num-problema)
                  (return (list line (ler-solucao num-problema)))))))))

(defun ler-solucao (num-problema)
  (with-open-file (file "C:\\Users\\Daniel\\Desktop\\Coisas do ips\\3∫Ano\\1∫Semestre\\IA\\Proj\\Dots-Boxes_IA\\Problemas\\solucoes.dat" :direction :input)
      (let ((line-number 0))
        (loop for line = (read file nil)
              while line do
                (incf line-number)
                (when (= line-number num-problema)
                  (return line))))))

;;; Output - escrita do estado do problema
;;
(defun escrever-no (no &optional (g t))
"Permite escrever um no, por defeito no ecra."
  (format g "~A" no))

 
(defun escreve-no (no)
 "Permite escrever no ecra um no do problema."
  (progn
     (format t "| A: ~a | B: ~a | G: ~a |~%" (vasilha-a-conteudo no) (vasilha-b-conteudo no) (no-profundidade no))
     (format t "Pai: ~a ~%" (no-pai no))
  ))

(defun escreve-lista-nos (lista)
  "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
  (cond
   ((null lista) nil)
   (T (progn (escreve-no (car lista)) (escreve-lista-nos (cdr lista))))))

(defun mostrar-solucao (no-solucao abertos fechados)
  (cond ((null no-solucao) (format t "Numero de n√≥s gerados: ~A | Numero de n√≥s expandidos: ~A" (+ (length abertos)(length fechados)) (length fechados)))
        (t (progn
             (format t "Estado: ~A | Profundidade: ~A | Heuristica: ~A | Custo: ~A" (no-estado no-solucao) (no-profundidade no-solucao) (no-heuristica no-solucao) (no-custo no-solucao))
             (mostrar-solucao (no-pai no-solucao) abertos fechados)))))

;; An√°lise de resultados
(defun penetrancia (comprimento-objetivo num-nos-gerados)
  (/ comprimento-objetivo num-nos-gerados))

(defun bisseccao ()
  )


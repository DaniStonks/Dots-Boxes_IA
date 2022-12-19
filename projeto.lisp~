;;;; projeto.lisp
;;;; Funcoes de interacao com o utilizador e de escrita e leitura de ficheiros
;;;; Autor: Daniel Baptista, Rafael Silva

(load "C:\\Users\\Daniel\\Desktop\\Coisas do ips\\3ºAno\\1ºSemestre\\IA\\Proj\\Dots-Boxes_IA\\puzzle.lisp")
(load "C:\\Users\\Daniel\\Desktop\\Coisas do ips\\3ºAno\\1ºSemestre\\IA\\Proj\\Dots-Boxes_IA\\procura.lisp")

;;; Inicializacao do programa
;; iniciar
(defun iniciar ()
  "Permite iniciar o programa, fazendo a leitura do teclado do estado inicial e do algoritmo a utilizar para procurar a soluÃ§Ã£o (neste caso a procura na profundidade ou na largura)"
  (setf *abertos* nil)
  (setf *fechados* nil)
  (let* ((problema (ler-problema))
         (no (cria-no (car problema)))
         (num-solucao (second problema))
         (algoritmo (ler-algoritmo))
         (heuristica (cond ((eql algoritmo 'a*) (ler-heuristica)) (t NIL)))
         (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999))))
    (cond
         ((equal algoritmo 'bfs) (mostrar-solucao (funcall algoritmo no 'no-solucaop 'sucessores (operadores) num-solucao *abertos* *fechados*)))
         ((equal algoritmo 'dfs) (mostrar-solucao (funcall algoritmo no 'no-solucaop 'sucessores (operadores) profundidade num-solucao *abertos* *fechados*)))
         ((equal algoritmo 'a*) (mostrar-solucao (funcall algoritmo no 'no-solucaop 'sucessores (operadores) heuristica num-solucao *abertos* *fechados*)))
         )))

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

;; ler-profundidade
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
    (with-open-file (file "C:\\Users\\Daniel\\Desktop\\Coisas do ips\\3ºAno\\1ºSemestre\\IA\\Proj\\Dots-Boxes_IA\\Problemas\\problemas.dat" :direction :input)
      (let ((line-number 0))
        (loop for line = (read file nil)
              while line do
                (incf line-number)
                (when (= line-number num-problema)
                  (return (list line (ler-solucao num-problema)))))))))

(defun ler-solucao (num-problema)
  (with-open-file (file "C:\\Users\\Daniel\\Desktop\\Coisas do ips\\3ºAno\\1ºSemestre\\IA\\Proj\\Dots-Boxes_IA\\Problemas\\solucoes.dat" :direction :input)
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
   (T (progn 
        (format t "Estado: ~A | Profundidade: ~A | Heuristica: ~A | Custo: ~A ~%" (no-estado lista) (no-profundidade lista) (no-heuristica lista) (no-custo lista)) 
        (escreve-lista-nos (no-pai lista))))))

(defun mostrar-solucao (no-solucao)
  (progn
    (escreve-lista-nos no-solucao)
    (format t "Numero de nós gerados: ~A | Numero de nós expandidos: ~A | Penetrância: ~A" (+ (length *abertos*)(length *fechados*)) (length *fechados*) (penetrancia (no-profundidade no-solucao) (+ (length *abertos*)(length *fechados*))))))

;; Analise de resultados
(defun penetrancia (comprimento-objetivo num-nos-gerados)
  (/ comprimento-objetivo num-nos-gerados))

(defun bisseccao ()
  )

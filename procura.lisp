;;;; procura.lisp
;;;; Funcoes dos metodos de procura
;;;; Autor: Daniel Baptista, Rafael Silva

;;; Funcoes auxiliares da procura
(defun ordenar-nos (lista-nos)
    (sort lista-nos 'no-menorp))

(defun no-menorp (no1 no2)
  (cond ((< (no-custo no1) (no-custo no2)) T)
        (t NIL)))

;;;Funcoes auxiliares dos metodos de procura
(defun abertos-bfs (abertos fechados sucessores)
  (append abertos (lista-elementos-diferentes sucessores (append fechados abertos))))

(defun abertos-dfs (abertos fechados sucessores)
  (let* ((novos-fechados (filtrar-nos fechados sucessores 'no-profundidade))
         (sucessores-novos (lista-elementos-diferentes sucessores (append novos-fechados abertos)))) 
    (list (append sucessores-novos abertos) novos-fechados)))

(defun abertos-e-fechados-a* (abertos fechados sucessores)
  "Esta funcao irá devolver uma lista com duas entradas, a primeira é pertencente aos novos nós abertos, depois de serem comparados com os nós sucessores. A segunda entrada é para os novos nós fechados, após tambem serem comparados com os nós sucessores"
  (let* ((novos-fechados (filtrar-nos fechados sucessores 'no-custo))
         (novos-abertos (filtrar-nos abertos sucessores 'no-custo))
         (sucessores-a-adicionar (lista-elementos-diferentes sucessores (append novos-fechados novos-abertos))))
    (list (ordenar-nos (append sucessores-a-adicionar novos-abertos)) novos-fechados)))

(defun no-existep (no lista algoritmo)
  (cond ((null lista) NIL)
        ((equal (car no) (car (car lista))) T)
        (t (no-existep no (cdr lista) algoritmo))))

(defun obter-no-estado-igual (no lista)
  (cond ((null lista) NIL)
        ((equal (car no) (car (car lista))) (car lista))
        (t (obter-no-estado-igual no (cdr lista)))))

(defun filtrar-nos (lista-nos sucessores f-custo)
  "Esta funcao irá receber uma lista de nos sucessores e outra lista que será de nos fechados e/ou abertos, de seguida ira filtrar essa lista, e dependendo do algoritmo retira os nos cujos estados estejam nos sucessores e tenham um custo ou profundidade menor"
  (apply #'append (mapcar (lambda (no)
                            (let ((no-igual-sucessor (obter-no-estado-igual no sucessores)))
                              (cond ((and (not (null no-igual-sucessor))(> (funcall f-custo no) (funcall f-custo no-igual-sucessor))) NIL)
                                    ((and (not (null no-igual-sucessor))(<= (funcall f-custo no) (funcall f-custo no-igual-sucessor))) no)
                                    (t no))))
                          lista-nos)))

(defun lista-elementos-diferentes (lista-a-filtrar lista-a-verificar)
"Esta funcao irá devolver uma lista com os elementos diferentes da primeira lista em comparacao com a segunda lista"
  (apply #'append (mapcar (lambda (no)
                            (cond ((null (no-estado no)) NIL)
                                  ((no-existep no lista-a-verificar 'a*) NIL)
                                  (t no)))
                          lista-a-filtrar)))


;;; Metodos de procura
;; procura na largura
(defun bfs (no f-objetivo f-sucessores operadores &optional num-solucao abertos fechados)
  (let* ((novos-fechados (cons no fechados))
        (novos-abertos (abertos-bfs abertos novos-fechados (funcall f-sucessores no operadores 'bfs nil)))
        (no-solucao (apply #'append (mapcar (lambda (no)
                                              (cond((funcall f-objetivo no num-solucao) no)
                                                   (t NIL)))
                                            novos-abertos))))
    (setf *abertos* novos-abertos)
    (setf *fechados* novos-fechados)
    (cond ((null novos-abertos) NIL)
          ((funcall f-objetivo no num-solucao) no)
          ((not (null no-solucao)) no-solucao)
          (t (bfs (car *abertos*) f-objetivo f-sucessores operadores num-solucao (cdr *abertos*) *fechados*)))))

;; procura na profundidade
(defun dfs (no f-objetivo f-sucessores operadores profundidade &optional num-solucao abertos fechados)
  (let* ((novos-sucessores-e-fechados (abertos-dfs abertos (cons no fechados) (funcall f-sucessores no operadores 'dfs nil 0 profundidade)))
         (novos-abertos (car novos-sucessores-e-fechados))
         (novos-fechados (car (cdr novos-sucessores-e-fechados)))
         (no-solucao (apply #'append (mapcar (lambda (no)
                                               (cond((funcall f-objetivo no num-solucao) no)
                                                    (t NIL)))
                                             novos-abertos))))
    (setf *abertos* novos-abertos)
    (setf *fechados* novos-fechados)
    (cond ((null novos-abertos) NIL)
          ((funcall f-objetivo no num-solucao) no)
          ((not (null no-solucao)) (cond ((/= (length no-solucao) 1) (car no-solucao))
                                         (t no-solucao)))
          (t (dfs (car *abertos*) f-objetivo f-sucessores operadores profundidade num-solucao (cdr *abertos*) *fechados*)))))

;;procura informada
(defun a* (no f-solucao f-sucessores operadores f-heuristica &optional num-solucao abertos fechados)
  (let* ((novos-abertos-fechados (abertos-e-fechados-a* abertos (cons no fechados) (funcall f-sucessores no operadores 'a* f-heuristica num-solucao)))
         (novos-abertos (car novos-abertos-fechados))
         (novos-fechados (car (cdr novos-abertos-fechados)))
         (no-solucao (cond((funcall f-solucao (car novos-abertos) num-solucao) (car novos-abertos))
                          (t NIL))))
    (setf *abertos* novos-abertos)
    (setf *fechados* novos-fechados)
    (cond ((null novos-abertos) NIL)
          ((funcall f-solucao no num-solucao) no)
          ((not (null no-solucao)) no-solucao)
          (t (a* (car *abertos*) f-solucao f-sucessores operadores f-heuristica num-solucao (cdr *abertos*) *fechados*)))))
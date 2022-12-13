;;;; procura.lisp
;;;; Funcoes dos metodos de procura
;;;; Autor: Daniel Baptista, Rafael Silva

;;; Funcoes auxiliares da procura
(defun ordenar-nos (lista-nos)
    (sort lista-nos 'no-menorp))

(defun no-menorp (no1 no2)
  (cond ((< (no-custo no1) (no-custo no2)) T)
        (t NIL)))

;;; sucessores
(defun novo-sucessor (no operador heuristica)
  (let* ((novo-estado (funcall operador (first no)))
        (nova-profundidade (1+ (second no)))
        (valor-heuristica (cond ((or(null heuristica)(null novo-estado)) 0)
                                (t (funcall heuristica novo-estado)))))
    (list novo-estado nova-profundidade valor-heuristica no)))

(defun sucessores (no operadores algoritmo heuristica &optional profundidade)
  (cond ((and (eq algoritmo 'dfs) (= (second no) profundidade)) NIL)
        ((null operadores) NIL)
        (t (cons (novo-sucessor no (first operadores) heuristica) (sucessores no (rest operadores) algoritmo heuristica profundidade)))))


;;;Funcoes auxiliares dos metodos de procura
(defun abertos-bfs (abertos fechados sucessores)
  (append abertos (apply #'append (mapcar (lambda (no)
                                            (cond ((null (no-estado no)) NIL)
                                                  ((no-existep no (append abertos fechados) 'bfs) NIL)
                                                  (t (list no))))
                                          sucessores))))

(defun abertos-dfs (abertos sucessores)
  (append sucessores abertos))

(defun no-existep (no lista algoritmo)
  (cond ((null lista) NIL)
        ((equal (car no) (car (car lista))) T)
        (t (no-existep no (cdr lista) algoritmo))))


;;; Metodos de procura
;; procura na largura
(defun bfs (no f-objetivo f-sucessores operadores &optional abertos fechados)
  (let* ((novos-fechados (cons no fechados))
        (novos-abertos (abertos-bfs abertos novos-fechados (funcall f-sucessores no operadores 'bfs nil)))
        (no-solucao (apply #'append (mapcar (lambda (no)
                                              (cond((funcall f-objetivo no) no)
                                                   (t NIL)))
                                            novos-abertos))))
    (cond ((null novos-abertos) NIL)
          ((funcall f-objetivo no) no)
          ((not (null no-solucao)) no-solucao)
          (t (bfs (car novos-abertos) f-objetivo f-sucessores operadores (cdr novos-abertos) novos-fechados)))))

;; procura na profundidade
(defun dfs (no f-objetivo f-sucessores operadores profundidade &optional abertos fechados)
  (let* ((novos-fechados (cons no fechados))
        (novos-abertos (abertos-dfs abertos novos-fechados (funcall f-sucessores no operadores 'dfs nil profundidade)))
        (no-solucao (apply #'append (mapcar (lambda (no)
                                              (cond((funcall f-objetivo no) no)
                                                   (t NIL)))
                                            novos-abertos))))
    (cond ((null novos-abertos) NIL)
          ((funcall f-objetivo no) no)
          ((not (null no-solucao)) no-solucao)
          (t (dfs (car novos-abertos) f-objetivo f-sucessores operadores profundidade (cdr novos-abertos) novos-fechados)))))

;;;; procura.lisp
;;;; Funções dos métodos de procura
;;;; Autor: Daniel Baptista, Rafael Silva

;;; sucessores
;; teste: (novo-sucessor (no-teste) 'encher-a)
;; resultado: ((3 2) 1 ((2 2) 0 NIL))
;; teste: (novo-sucessor (no-teste) 'transferir-a-b)
;; resultado: ((0 4) 1 ((2 2) 0 NIL))
;; teste: (novo-sucessor (cria-no '(3 5)) 'encher-a)
;; resultado: NIL
(defun novo-sucessor (no operador)
  (let ((novo-estado (funcall operador (first no)))
        (nova-profundidade (1+ (second no))))
    (list novo-estado nova-profundidade no)))

;; teste: (sucessores (no-teste) (operadores) 'bfs)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
;; teste: (sucessores (no-teste) (operadores) 'dfs 2)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
(defun sucessores (no operadores algoritmo &optional profundidade)
  (cond ((and (eq algoritmo 'dfs) (= (second no) profundidade)) NIL)
        ((null operadores) NIL)
        (t (cons (novo-sucessor no (first operadores)) (sucessores no (rest operadores) algoritmo profundidade)))))


;;;Algoritmos funções auxiliares
;;;abertos-bfs
(defun abertos-bfs (abertos sucessores)
  (append abertos sucessores))

;;;abertos-dfs
(defun abertos-dfs (abertos sucessores)
  (append sucessores abertos))

;;no-existep
(defun no-existep (no lista algoritmo)
  (cond ((null lista) NIL)
        ((equal (car no) (car (car lista))) T)
        (t (no-existep no (cdr lista) algoritmo))))

;;; Algoritmos
;; procura na largura
;; teste: (bfs (no-teste) 'no-solucaop 'sucessores (operadores) nil nil)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))
(defun bfs (no f-objetivo f-sucessores operadores &optional abertos fechados)
  (labels ((bfs-loop (abertos fechados)
             (cond ((null abertos) NIL) ;;se a lista de nós abertos estiver vazia, o algoritmo acaba
                   (t (let* ((no-expandir (first abertos)) ;no algoritmo bfs o nó a expandir é sempre o primeiro da lista de abertos
                             (novos-fechados (append fechados (list no-expandir)))
                             (sucessores-filtrados (remove nil (mapcar (lambda (no) ;verifica se os nós gerados já existem nos nós fechados
                                                                         (cond ((no-existep no novos-fechados 'bfs) nil) 
                                                                               (t no))) ;apenas são retornados os nós não presentes nos nós fechados
                                                                       (funcall f-sucessores no-expandir operadores 'bfs))))
                             (novos-abertos (abertos-dfs (cdr abertos) sucessores-filtrados)) ;expande o no escolhido, remove o mesmo nó da lista de nos abertos e junta o restante da lista de abertos com os novos sucessores da expansão
                             (no-objetivo (apply #'append (mapcar (lambda (no) ;verifica se na lista de nós abertos algum deles é o nó solução, a seguir retira os NILs da lista
                                                                    (cond ((funcall f-objetivo no) no) ;se o no a verificar for a solução retorna o no
                                                                          (t NIL)))
                                                                  novos-abertos))))
                        (cond ((not (null no-objetivo)) no-objetivo) ;se a lista no-objetivo nao estiver vazia apos ter sido retirado os NILs, significa que foi encontrado o nó solução e este é devolvido, acabando o algoritmo
                              (t (bfs-loop novos-abertos novos-fechados)))))))) ;adiciona o nó expandido a lista de nós fechados
    (bfs-loop (list (append abertos no)) fechados)))

;; procura na profundidade
;; teste: (dfs (no-teste) 'no-solucaop 'sucessores (operadores) 10)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))
(defun dfs (no f-objetivo f-sucessores operadores profundidade &optional abertos fechados)
  (labels ((dfs-loop (abertos fechados)
             (cond ((null abertos) NIL) ;;se a lista de nós abertos estiver vazia, o algoritmo acaba
                   (t (let* ((no-expandir (first abertos)) ;no algoritmo bfs o nó a expandir é sempre o primeiro da lista de abertos
                             (novos-fechados (append fechados (list no-expandir)))
                             (sucessores-filtrados (remove nil (mapcar (lambda (no) ;verifica se os nós gerados já existem nos nós fechados
                                                                         (cond ((no-existep no novos-fechados 'dfs) nil) 
                                                                               (t no))) ;apenas são retornados os nós não presentes nos nós fechados
                                                                       (funcall f-sucessores no-expandir operadores 'dfs profundidade))))
                             (novos-abertos (abertos-dfs (cdr abertos) sucessores-filtrados)) ;expande o no escolhido, remove o mesmo nó da lista de nos abertos e junta o restante da lista de abertos com os novos sucessores da expansão
                             (no-objetivo (apply #'append (mapcar (lambda (no) ;verifica se na lista de nós abertos algum deles é o nó solução, a seguir retira os NILs da lista
                                                                    (cond ((funcall f-objetivo no) no) ;se o no a verificar for a solução retorna o no
                                                                          (t NIL)))
                                                                  novos-abertos))))
                        (cond ((not (null no-objetivo)) no-objetivo) ;se a lista no-objetivo nao estiver vazia apos ter sido retirado os NILs, significa que foi encontrado o nó solução e este é devolvido, acabando o algoritmo
                              (t (dfs-loop novos-abertos novos-fechados)))))))) ;adiciona o nó expandido a lista de nós fechados
    (dfs-loop (list (append abertos no)) fechados)))

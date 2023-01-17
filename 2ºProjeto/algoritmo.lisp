;;;; procura.lisp
;;;; Funcoes dos metodos de procura
;;;; Autor: Daniel Baptista, Rafael Silva


;;;;;;;;;;;;;;;;;;;;;;;;
;; Funções auxiliares ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun trocar-jogador (jogador)
  (cond ((= jogador 1) 2)
        (t 1)))

;;; Funcoes auxiliares da procura
(defun ordenar-nos (lista-nos)
    (sort lista-nos 'no-menorp))

(defun no-menorp (no1 no2)
  (cond ((< (no-custo no1) (no-custo no2)) T)
        (t NIL)))

(defun no-existep (no lista algoritmo)
  (cond ((null lista) NIL)
        ((equal (car no) (car (car lista))) T)
        (t (no-existep no (cdr lista) algoritmo))))

(defun obter-no-estado-igual (no lista)
  (cond ((null lista) NIL)
        ((equal (car no) (car (car lista))) (car lista))
        (t (obter-no-estado-igual no (cdr lista)))))

(defun filtrar-nos (lista-nos sucessores f-custo)
  "Esta funcao irÃ¡ receber uma lista de nos sucessores e outra lista que serÃ¡ de nos fechados e/ou abertos, de seguida ira filtrar essa lista, e dependendo do algoritmo retira os nos cujos estados estejam nos sucessores e tenham um custo ou profundidade menor"
  (remove nil (mapcar (lambda (no)
                            (let ((no-igual-sucessor (obter-no-estado-igual no sucessores)))
                              (cond ((and (not (null no-igual-sucessor))(> (funcall f-custo no) (funcall f-custo no-igual-sucessor))) NIL)
                                    ((and (not (null no-igual-sucessor))(<= (funcall f-custo no) (funcall f-custo no-igual-sucessor))) no)
                                    (t no))))
                          lista-nos)))

(defun abertos-dfs (abertos fechados sucessores)
  (let* ((novos-fechados (filtrar-nos fechados sucessores 'no-profundidade))
         (sucessores-novos (lista-elementos-diferentes sucessores (append novos-fechados abertos)))) 
    (list (append sucessores-novos abertos) novos-fechados)))

(defun lista-elementos-diferentes (lista-a-filtrar lista-a-verificar)
"Esta funcao irÃ¡ devolver uma lista com os elementos diferentes da primeira lista em comparacao com a segunda lista"
  (remove nil (mapcar (lambda (no)
                            (cond ((null (no-estado no)) NIL)
                                  ((no-existep no lista-a-verificar 'a*) NIL)
                                  (t no)))
                          lista-a-filtrar)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Metodos de procura ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; procura na profundidade
(defun dfs (no f-objetivo f-sucessores operadores profundidade &optional num-solucao abertos fechados)
  (let* ((novos-sucessores-e-fechados (abertos-dfs abertos (cons no fechados) (funcall f-sucessores no operadores 'dfs nil 0 profundidade)))
         (novos-abertos (car novos-sucessores-e-fechados))
         (novos-fechados (car (cdr novos-sucessores-e-fechados)))
         (no-solucao (remove nil (mapcar (lambda (no)
                                               (cond((funcall f-objetivo no num-solucao) no)
                                                    (t NIL)))
                                             novos-abertos))))
    (setf *abertos* novos-abertos)
    (setf *fechados* novos-fechados)
    (cond ((null novos-abertos) NIL)
          ((funcall f-objetivo no num-solucao) no)
          ((not (null no-solucao)) (cond ((/= (length no-solucao) 1) (car no-solucao))
                                         (t (car no-solucao))))
          (t (dfs (car *abertos*) f-objetivo f-sucessores operadores profundidade num-solucao (cdr *abertos*) *fechados*)))))


(defun minimax (no operadores sucessores avaliacao profundidade jogador)
  (cond ((= 0 profundidade) (heuristica no))
        (t (let ((nos-filhos (sucessores no operadores jogador)))
             (cond ((= jogador 1) (reduce 'max (mapcar (lambda (filho) (minimax filho operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador))) nos-filhos)))
                   (t (reduce 'min (mapcar (lambda (filho) (minimax filho operadores sucessores avaliacao (1- profundidade) (trocar-jogador jogador))) nos-filhos))))))))
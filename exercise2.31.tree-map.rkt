(load "util.rkt")
(define (tree-map f tree)
  (define (g node subtree)
    (if (list? node)
        (cons (tree-map f node) subtree)
        (cons (f node) subtree)
    ))
  (foldr g '() tree)
  )

(define (square-tree xs) (tree-map square xs))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;(1 (4 (9 16) 25) (36 49))
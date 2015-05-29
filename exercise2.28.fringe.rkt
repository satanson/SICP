(define (fringe xs)
  (define (iter xs ys)
    (cond
      ((null? xs) 
       ys)
      ((not (pair? (car xs)))
       (cons (car xs) (iter (cdr xs) ys)))
      (else
       (append (fringe (car xs)) (fringe (cdr xs)) ys))
      ))
  (iter xs '())
  )

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;(1 2 3 4)

(fringe (list x x))
;(1 2 3 4 1 2 3 4)


  
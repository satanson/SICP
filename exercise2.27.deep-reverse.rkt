(define (deep-reverse xs)
  (define (iter xs ys)
    (cond
      ((null? xs) ys)
      ((not (pair? (car xs))) (iter (cdr xs) (cons (car xs) ys)))
      (else (iter (cdr xs) (cons (deep-reverse (car xs)) ys)))
      )
    )
  (iter xs '()))


(define x (list (list 1 2) (list 3 4)))

x
;((1 2) (3 4))

(reverse x)
;((3 4) (1 2))

(deep-reverse x)
;((4 3) (2 1))


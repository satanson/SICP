(load "util.rkt")

(define (count-leaves-v1 t)
  (cond ((null? t) 0)
        ((not (pair? (car t))) (+ 1 (count-leaves-v1 (cdr t))))
        (else (+ (count-leaves-v1 (car t)) (count-leaves-v1 (cdr t))))))

(count-leaves-v1 '())
(count-leaves-v1 '(1))
(count-leaves-v1 '(1 2 3 4))
(count-leaves-v1 '((1 2) 3))

(define (count-leaves-v2 t)
  (define (f node)
    (cond ((not (list? node)) 1)
          ((null? node) 0)
          (else count-leaves-v2 node)))
  (map f t))
  
(count-leaves-v1 '((1 (2 3) 4 (5 (6 (7 8 9))))))
(count-leaves-v2 '((1 (2 3) 4 (5 (6 (7 8 9))))))
#lang racket
(require scheme/mpair)

(define (make-set . xs)
  (define set (apply mlist xs))
  (define (contains? e)
    (if (null? (filter (lambda (x)(eq? x e)) (mlist->list set)))
        #f
        #t))
  (define (add! e)
    (if (contains? e) '() (set! set (mcons e set))))
  (define (->string)
    (begin 
      (display (mlist->list set))
      (newline)))
  (lambda (m)
    (cond ((eq? m 'contains?) contains?)
          ((eq? m 'add!) add!)
          ((eq? m '->string) ->string)
          (else (error "Not Implemented Method!")))))

(define S (make-set 1 2 3))
((S 'contains?) 1)
((S 'contains?) 4)
((S 'add!) 4)
((S 'add!) 1)
((S '->string))
((S 'add!) 1)
((S 'add!) 1)
((S '->string))


(define one-to-three (list 1 2 3))
one-to-three
(set! one-to-three (cons 0 one-to-three))
one-to-three

((lambda(s e)(set! s (cons e s))) one-to-three 0)
one-to-three

(equal? one-to-three one-to-three)
((lambda(x y)(equal? x y)) one-to-three one-to-three)
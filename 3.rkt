#lang racket
(define (foobar)
  (let ((_length 0))
    (lambda(m)
      (cond ((eq? m 'length) _length)
            ((eq? m 'inc) (set! _length (+ 1 _length)))))))

(define x (foobar))

(x 'length)
(x 'inc)
(x 'length)

(set! y 10)
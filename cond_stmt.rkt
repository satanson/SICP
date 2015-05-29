(define (MAX1 x y)(if (< x y) y x))
(define (MAX2 x y)(cond ((< x y) => y) (else x)))

(cond ((assv 'b '((a 1) (b 2))) => cadr) (else #f))

(define (list+ e1 . rest) (cons e1 rest))

(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
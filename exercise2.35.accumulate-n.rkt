(load "util.rkt")
(define (foldr-n op z seqs)
  (if (null? (car seqs))
      nil
      (cons (foldr op z (map car seqs))
            (foldr-n op z (map cdr seqs)))))

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (rv) (dot-product rv v)) m))

(define (transpose m)
  (foldr-n cons nil m))

(transpose '((1 2 3 4) (5 6 7 8) (9 10 11 12)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rv)(matrix-*-vector n rv)) m)))
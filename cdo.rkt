(load "util.rkt")
(define (make-rat n d)
  (let* (
         (p (gcd n d))
         (n (/ n p))
         (d (/ d p)))
    (if (or
         (and (negative? n) (positive? d))
         (and (positive? n) (negative? d)))
        (cons (* -1 (abs n)) (abs d))
        (cons (abs n) (abs d)))))
(comment
(make-rat 3 4)
(make-rat 3 6)
(make-rat -3 6)
(make-rat -3 -6)
(make-rat 3 -6)
(make-rat -3 0)
(make-rat 0 0)
(make-rat 0 1)
)

(define (numer x) (car x))
(define (denom x) (cdr x))
(comment
(define r (make-rat 3 4))
(numer r)
(denom r)
)

(define (print-rat r)
  (display (numer r))
  (display "/")
  (display (denom r)))

(comment
(define r (make-rat 3 4))
(print-rat r)
)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define twice (make-rat 1 2))
(define third (make-rat 1 3))
(define fourth (make-rat 1 4))
(define fifth (make-rat 1 5))
(define sixth (make-rat 1 6))

;(print-rat (add-rat twice third))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

;(print-rat (sub-rat twice third))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
;(print-rat (mul-rat twice sixth))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (eq-rat x y)
  (= (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (cons1 x y)
  (lambda (m) (m x y)))
(define (car1 z)
  (z (lambda (p q) p)))
(define (cdr1 z)
  (z (lambda (p q) q)))

(define z1 (cons1 1 2))
(car1 z1)
(cdr1 z1)

(define (cons2 n m)
  (* (expt 2 n) (expt 3 m)))
(define (factor-num z p)
  (define (iter z k)
    (if (zero? (modulo z p))
        (iter (/ z p) (+ 1 k))
        k))
  (iter z 0))
(define (car2 z) (factor-num z 2))
(define (cdr2 z) (factor-num z 3))



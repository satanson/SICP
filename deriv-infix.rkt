(load "util.rkt")
(define (=number? exp num)
  (and (number? exp) (= exp num)))


 (define (make-product m1 m2)
   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
         ((=number? m1 1) m2)
         ((=number? m2 1) m1)
         ((and (number? m1) (number? m2)) (* m1 m2))
         (else (list m1 '*  m2))))

 
 (define (make-sum a1 a2)
   (cond ((=number? a1 0) a2)
         ((=number? a2 0) a1)
         ((and (number? a1) (number? a2)) (+ a1 a2))
         (else (list a1 '+ a2))))


;The variables are symbols. They are identified by the primitive predicate symbol?:
(define (variable? x) (symbol? x))

;Two variables are the same if the symbols representing them are eq?:
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;Sums and products are constructed as lists:
;(define (make-sum a1 a2) (list '+ a1 a2))

;(define (make-product m1 m2) (list '* m1 m2))

;A sum is a list whose first element is the symbol +:
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

;The addend is the second item of the sum list:
(define (addend s) (car s))

;The augend is the third item of the sum list:
(define (augend s) 
  (if (unary? (cddr s)) (caddr s) (cddr s)))

;A product is a list whose first element is the symbol *:
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

;The multiplier is the second item of the product list:
(define (multiplier p) (car p))

;The multiplicand is the third item of the product list:
(define (multiplicand p)
  (if (unary? (cddr p)) (caddr p) (cddr p)))

(define (make-exponent b e) 
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))
(comment 
 
 (make-exponent 'x 0)
 (make-exponent 'x 1)
 (make-exponent 'x 2)
 
 )

(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (exponent? x) (eq? (car x) '**))
;(comment
 (define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
          (if (same-variable? exp var) 1 0))
         ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
         ((product? exp)
          (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
         ((exponent? exp)
          (let ((b (base exp))
                (e (exponent exp)))
            (cond ((=number? e 0) 0)
                  ((=number? e 1) (deriv b var))
                  (else (make-product e (make-product (deriv b var) (make-exponent b (- e 1))))))))
         (else
          (display "unknown expression type -- DERIV" exp))))
 
 (deriv '(** x 10) 'x)
 ;)
 
 (deriv '(
          (10 * (** x 3))
          + 
          (-9 * (** x 2))
          +
          (3 * (** x 1))
          +
          (-7 * (** x 0))) 
        'x)
(load "util.rkt")

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b))
             (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b))
             (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect x a)
  (make-vect (* x (xcor-vect a))
             (* x (ycor-vect a))))

(comment
 
(define a (make-vect 1 2))
(define b (make-vect 3 4))
(add-vect a b)
(sub-vect a b)
(scale-vect 4 a)

)

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame f)(car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

(define (frame-coord-map frame)
   (lambda (v)
     (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))


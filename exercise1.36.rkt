(load "util.rkt")

(comment
(let (
      (a 1)
      (b 2))
  (display 1)
  (newline)
  (display 2))
)
(comment
(define fai (+ 0.0 (fixed-point-verbose (lambda (x) (+ 1 (/ 1 x))) 1.0 println)))
fai
)

(fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 2.0 println)
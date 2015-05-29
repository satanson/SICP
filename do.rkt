(do (
     (vec (make-vector 5)) 
     (i 0 (+ i 1))
    )
  ((= i 5) vec)
  (vector-set! vec i i)
  )

(do (
     (i 1 (+ i 1))
     (s 0)
     )
  ((> i 100) s)
  (set! s (+ s i))
  )

(let () 1 2 3)
(begin 1 2 3)




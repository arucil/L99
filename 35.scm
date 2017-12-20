(load "prelude.scm")
(load "31.scm")

;;; P35

(define (prime-factors n)
  (let f ([i 2] [n n])
    (cond
     [(= 1 n) '()]
     [(and (zero? (remainder n i))
           (prime? i))
      (cons i (f i (quotient n i)))]
     [else
      (f (add1 i) n)])))

(test (prime-factors 3) '(3))
(test (prime-factors 16) '(2 2 2 2))
(test (prime-factors 63) '(3 3 7))

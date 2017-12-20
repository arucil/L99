(load "prelude.scm")

;;; P31

(define (prime? n)
  (let f ([i 2])
    (cond
     [(>= i n) #t]
     [(zero? (remainder n i)) #f]
     [else (f (add1 i))])))

(test (prime? 2) #t)
(test (prime? 7) #t)
(test (prime? 10) #f)
(test (prime? 65537) #t)
(test (prime? 143) #f)

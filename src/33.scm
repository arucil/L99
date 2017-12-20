(load "prelude.scm")

;;; P33

(define (coprime? a b)
  (= 1 (gcd a b)))

(test (coprime? 10 15) #f)
(test (coprime? 11 13) #t)

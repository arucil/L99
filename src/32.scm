(load "prelude.scm")

;;; P32

(define (gcd a b)
  (cond
   [(zero? a) b]
   [(zero? b) a]
   [else
    (gcd b (remainder a b))]))

(test (gcd 10 15) 5)
(test (gcd 15 10) 5)
(test (gcd 11 13) 1)
(test (gcd 36 63) 9)

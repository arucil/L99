(load "prelude.scm")
(load "22.scm")
(load "23.scm")

;;; P24

(define (lotto-select n m)
  (rnd-select (range 1 m) n))

(test (begin (init-random! 0) (lotto-select 6 49)) '(32 7 35 18 37 16))

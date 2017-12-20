(load "prelude.scm")
(load "17.scm")

;;; P18

(define (slice ls i j)
  (car (split (cadr (split ls (1- i))) (- j i -1))))

(test (slice '(a b c d e f g h i j) 3 7) '(c d e f g))

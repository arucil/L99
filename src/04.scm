(load "prelude.scm")

;;; P04

(define (my-length ls)
  (if (null? ls)
      0
      (1+ (my-length (cdr ls)))))

(test (my-length '()) 0)
(test (my-length '(a)) 1)
(test (my-length '(a b c d)) 4)

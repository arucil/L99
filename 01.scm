(load "prelude.scm")

;;; P01

(define (my-last ls)
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (my-last (cdr ls))))

(test (my-last '(a b c d)) '(d))
(test (my-last '(a)) '(a))
(test (my-last '()) '())


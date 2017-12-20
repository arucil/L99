(load "prelude.scm")

;;; P02

(define (my-but-last ls)
  (if (or (null? ls)
          (null? (cdr ls))
          (null? (cddr ls)))
      ls
      (my-but-last (cdr ls))))

(test (my-but-last '(a b c d)) '(c d))
(test (my-but-last '(c d)) '(c d))
(test (my-but-last '(d)) '(d))
(test (my-but-last '()) '())

(load "prelude.scm")

;;; P14

(define (dupli ls)
  (if (null? ls)
      '()
      (cons (car ls)
            (cons (car ls)
                  (dupli (cdr ls))))))

(test (dupli '()) '())
(test (dupli '(a)) '(a a))
(test (dupli '(a b)) '(a a b b))
(test (dupli '(a a)) '(a a a a))
(test (dupli '(a b c c d)) '(a a b b c c c c d d))

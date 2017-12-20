(load "prelude.scm")

;;; P05

(define (my-reverse ls)
  (let f ([ls ls] [r '()])
    (if (null? ls)
        r
        (f (cdr ls) (cons (car ls) r)))))

(test (my-reverse '()) '())
(test (my-reverse '(a)) '(a))
(test (my-reverse '(a b)) '(b a))
(test (my-reverse '(a b c d e)) '(e d c b a))

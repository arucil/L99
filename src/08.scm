(load "prelude.scm")

;;; P08

(define (compress ls)
  (cond
   [(null? ls) '()]
   [(null? (cdr ls)) ls]
   [(equal? (car ls) (cadr ls)) (compress (cdr ls))]
   [else (cons (car ls) (compress (cdr ls)))]))

(test (compress '()) '())
(test (compress '(a)) '(a))
(test (compress '(a a)) '(a))
(test (compress '(a a b)) '(a b))
(test (compress '(a a b b)) '(a b))
(test (compress '(a a a a b c c a a d e e e e)) '(a b c a d e))

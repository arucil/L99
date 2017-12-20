(load "prelude.scm")


;;; P07

(define (flatten ls)
  (cond
   [(null? ls) '()]
   [(pair? ls) (append (flatten (car ls))
                       (flatten (cdr ls)))]
   [else (list ls)]))

(test (flatten '()) '())
(test (flatten '(a)) '(a))
(test (flatten '((a))) '(a))
(test (flatten '(((a)))) '(a))
(test (flatten '(((a) b))) '(a b))
(test (flatten '(a (b (c d) e))) '(a b c d e))

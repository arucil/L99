(load "prelude.scm")

;;; P20

(define (remove-at ls n)
  (cond
   [(null? ls) '()]
   [(= 1 n) (cdr ls)]
   [else (cons (car ls)
               (remove-at (cdr ls) (1- n)))]))

(test (remove-at '() 1) '())
(test (remove-at '(a) 1) '())
(test (remove-at '(a b c) 2) '(a c))
(test (remove-at '(a b c) 3) '(a b))
(test (remove-at '(a b c) 4) '(a b c))

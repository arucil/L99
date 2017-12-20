(load "prelude.scm")

;;; P03

(define (element-at ls i)
  (cond
   [(null? ls) '()]
   [(= 1 i) (car ls)]
   [else (element-at (cdr ls) (- i 1))]))

(test (element-at '(a b c d e) 3) 'c)
(test (element-at '(a b c d e) 1) 'a)
(test (element-at '(a b c d e) 5) 'e)
(test (element-at '(a b c d e) 6) '())

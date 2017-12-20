(load "prelude.scm")

;;; P62B

(define (atlevel t n)
  (cond
   [(null? t) '()]
   [(= 1 n) (list (car t))]
   [else
    (append (atlevel (cadr t) (sub1 n))
            (atlevel (caddr t) (sub1 n)))]))

(test (atlevel '(x () ()) 1)
      '(x))
(test (atlevel '(x (a (b () ()) ())
                   (c () (d (e () ()) ())))
               2)
      '(a c))
(test (atlevel '(x (a (b () ()) ())
                   (c () (d (e () ()) ())))
               3)
      '(b d))

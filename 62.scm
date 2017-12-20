(load "prelude.scm")

;;; P62

(define (internals t)
  (cond
   [(null? t) '()]
   [(and (null? (cadr t))
         (null? (caddr t)))
    '()]
   [else
    (append (internals (cadr t))
            (list (car t))
            (internals (caddr t)))]))

(test (internals '())
      '())
(test (internals '(x () ()))
      '())
(test (internals '(x (y () ()) ()))
      '(x))
(test (internals '(x (a () (b () ())) (c (d () ()) ())))
      '(a x c))

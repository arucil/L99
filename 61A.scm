(load "prelude.scm")

;;; P61A

(define (leaves t)
  (cond
   [(null? t) '()]
   [(and (null? (cadr t))
         (null? (caddr t)))
    (list (car t))]
   [else
    (append (leaves (cadr t))
            (leaves (caddr t)))]))

(test (leaves '())
      '())
(test (leaves '(x () ()))
      '(x))
(test (leaves '(x (a () ()) (b (c () ()) ())))
      '(a c))

(load "prelude.scm")

;;; P61

(define (count-leaves t)
  (cond
   [(null? t) 0]
   [(and (null? (cadr t))
         (null? (caddr t)))
    1]
   [else
    (+ (count-leaves (cadr t))
       (count-leaves (caddr t)))]))

(test (count-leaves '())
      0)
(test (count-leaves '(x () ()))
      1)
(test (count-leaves '(x (x () ()) ()))
      1)

(load "prelude.scm")

;;; P64

(define (layout-binary-tree t)
  (let ([x 0])
    (let f ([t t] [y 1])
      (if (null? t)
          '()
          (let* ([l (f (cadr t) (add1 y))]
                 [x (begin
                      (set! x (add1 x))
                      x)]
                 [r (f (caddr t) (add1 y))])
            (list (car t) x y l r))))))

(test (layout-binary-tree '(x () ()))
      '(x 1 1 () ()))
(test (layout-binary-tree '(x (a () (c () ())) ()))
      '(x 3 1 (a 1 2 () (c 2 3 () ())) ()))
(test (layout-binary-tree
       '(n (k (c (a () ())
                 (h (g (e () ())
                       ())
                    ()))
              (m () ()))
           (u (p ()
                 (s (q () ())
                    ()))
              ())))
      '(n 8 1
          (k 6 2
             (c 2 3
                (a 1 4 () ())
                (h 5 4
                   (g 4 5
                      (e 3 6 () ())
                      ())
                   ()))
             (m 7 3 () ()))
          (u 12 2
             (p 9 3
                ()
                (s 11 4
                   (q 10 5 () ())
                   ()))
             ())))

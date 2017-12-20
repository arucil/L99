(load "prelude.scm")

;;; P65

(define (height t)
  (if (null? t)
      0
      (add1 (max (height (cadr t))
                 (height (caddr t))))))

(define (layout-binary-tree-2 t)
  (define (get-x0 t d)
    (cond
     [(null? t) 1]
     [(null? (cadr t)) 1]
     [else
      (+ d (get-x0 (cadr t) (/ d 2)))]))
  (let* ([d (expt 2 (- (height t) 2))]
         [x0 (get-x0 t d)])
    (let f ([t t] [x x0] [y 1] [d d])
      (if (null? t)
          '()
          (list (car t) x y
                (f (cadr t) (- x d) (add1 y) (/ d 2))
                (f (caddr t) (+ x d) (add1 y) (/ d 2)))))))

(test (layout-binary-tree-2
       '(x () ()))
      '(x 1 1 () ()))
(test (layout-binary-tree-2
       '(x (y () (z () ())) ()))
      '(x 3 1
          (y 1 2
             ()
             (z 2 3 () ()))
          ()))
(test (layout-binary-tree-2
       '(n (k (c (a () ())
                 (e (d () ())
                    (g () ())))
              (m () ()))
           (u (p ()
                 (q () ()))
              ())))
      '(n 15 1
          (k 7 2
             (c 3 3
                (a 1 4 () ())
                (e 5 4
                   (d 4 5 () ())
                   (g 6 5 () ())))
             (m 11 3 () ()))
          (u 23 2
             (p 19 3
                ()
                (q 21 4 () ()))
             ())))

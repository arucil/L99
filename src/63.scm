(load "prelude.scm")

;;; P63

(define (complete-binary-tree n)
  (let f ([i 1])
    (if (> i n)
        '()
        (list i
              (f (* i 2))
              (f (add1 (* i 2)))))))

(test (complete-binary-tree 3)
      '(1 (2 () ())
          (3 () ())))
(test (complete-binary-tree 6)
      '(1 (2 (4 () ())
             (5 () ()))
          (3 (6 () ())
             ())))

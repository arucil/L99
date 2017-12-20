(load "prelude.scm")
(load "55.scm")
(load "56.scm")

;;; P58

(define (sym-cbal-trees n)
  (filter symmetric-tree? (cbal-trees n)))

(test (sym-cbal-trees 1)
      '((x () ())))
(test (sym-cbal-trees 2)
      '())
(test (sym-cbal-trees 3)
      '((x (x () ())
           (x () ()))))
(test (sym-cbal-trees 5)
      '((x (x () (x () ()))
           (x (x () ()) ()))
        (x (x (x () ()) ())
           (x () (x () ())))))

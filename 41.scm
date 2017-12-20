(load "prelude.scm")
(load "40.scm")

;;; P41

(define (goldbach-list a b)
  (let f ([a (if (even? a)
                 a
                 (add1 a))])
    (if (> a b)
        '()
        (cons (cons a (goldbach a))
              (f (+ 2 a))))))

(test (goldbach-list 9 20) '((10 3 7)
                             (12 5 7)
                             (14 3 11)
                             (16 3 13)
                             (18 5 13)
                             (20 3 17)))

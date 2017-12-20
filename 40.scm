(load "prelude.scm")
(load "31.scm")

;;; P40

(define (goldbach n)
  (let f ([a 2])
    (if (and (prime? a)
             (prime? (- n a)))
        (list a (- n a))
        (f (add1 a)))))

(test (goldbach 20) '(3 17))
(test (goldbach 992) '(73 919))

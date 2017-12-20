(load "prelude.scm")
(load "31.scm")

;;; P39

(define (prime-range a b)
  (let f ([a a])
    (cond
     [(> a b) '()]
     [(prime? a) (cons a (f (+ 2 a)))]
     [else (f (add1 a))])))

(test (prime-range 3 3) '(3))
(test (prime-range 3 10) '(3 5 7))
(test (prime-range 3 17) '(3 5 7 11 13 17))

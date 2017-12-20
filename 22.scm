(load "prelude.scm")

;;; P22

(define (range a b)
  (if (> a b)
      (reverse (range b a))
      (let f ([a a])
        (if (> a b)
            '()
            (cons a (f (1+ a)))))))

(test (range 4 9) '(4 5 6 7 8 9))
(test (range 1 1) '(1))
(test (range 9 4) '(9 8 7 6 5 4))

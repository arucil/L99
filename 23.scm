(load "prelude.scm")
(load "03.scm")
(load "20.scm")

;;; P23

(define (rnd-select ls n)
  (if (zero? n)
      '()
      (let ([i (1+ (random (length ls)))])
        (cons (element-at ls i)
              (rnd-select (remove-at ls i) (1- n))))))

(test (begin (init-random! 0) (rnd-select '(a b c d e f g) 3)) '(d a g))

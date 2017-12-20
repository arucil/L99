;;; P71

(load "prelude.scm")

(define (ipl t)
  (let f ([t t] [l 0])
    (if (symbol? t)
        l
        (apply
         +
         l
         (map
          (lambda (x)
            (f x (add1 l)))
          (cdr t))))))


(test (ipl 'x)
      0)
(test (ipl '(x f g))
      2)
(test (ipl '(x (f g)))
      3)
(test (ipl '(a (f g) c (b d e)))
      9)

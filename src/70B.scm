;;; P70B

(load "prelude.scm")

(define (tree? t)
  (or (symbol? t)
      (and (list? t)
           (>= (length t) 1)
           (symbol? (car t))
           (for-all tree? (cdr t)))))

(test (tree? 'x)
      #t)
(test (tree? '(x))
      #t)
(test (tree? '())
      #f)
(test (tree? '(x (y z) a (b (c))))
      #t)
(test (tree? '(x (y z) a ((c))))
      #f)

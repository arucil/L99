(load "prelude.scm")
(load "17.scm")

;;; P19

(define (rotate ls n)
  (if (negative? n)
      (let ([r (split ls (+ (length ls) n))])
        (append (cadr r) (car r)))
      (let ([r (split ls n)])
        (append (cadr r) (car r)))))

(test (rotate '(a b c d e f g h) 3) '(d e f g h a b c))
(test (rotate '(a b c d e f g h) -2) '(g h a b c d e f))
(test (rotate '(a b c d e f g h) 0) '(a b c d e f g h))

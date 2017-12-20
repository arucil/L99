;;; P70C

(load "prelude.scm")

(define (count-nodes t)
  (if (symbol? t)
      1
      (apply + 1 (map count-nodes (cdr t)))))

(test (count-nodes 'x)
      1)
(test (count-nodes '(a b))
      2)
(test (count-nodes '(a (b c)))
      3)
(test (count-nodes '(b d e))
      3)
(test (count-nodes '(a (f g) c (b d e)))
      7)

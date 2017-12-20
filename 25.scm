(load "prelude.scm")
(load "23.scm")

;;; P25

(define (rnd-permu ls)
  (rnd-select ls (length ls)))

(test (begin (init-random! 0) (rnd-permu '(a b c d e f g))) '(d a g b f c e))

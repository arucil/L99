(load "prelude.scm")

;;; P26

(define (combination n ls)
  (cond
   [(zero? n) '(())]
   [(> n (length ls)) '()]
   [else
    (append (map (lambda (x)
                   (cons (car ls) x))
                 (combination (sub1 n) (cdr ls)))
            (combination n (cdr ls)))]))

(test (combination 0 '()) '(()))
(test (combination 1 '()) '())
(test (combination 1 '(a)) '((a)))
(test (combination 1 '(a b)) '((a) (b)))
(test (combination 1 '(a b c)) '((a) (b) (c)))
(test (combination 2 '(a b c)) '((a b) (a c) (b c)))
(test (combination 3 '(a b c)) '((a b c)))
(test (combination 2 '(a b c d)) '((a b) (a c) (a d) (b c) (b d) (c d)))

(load "prelude.scm")

;;; P17

(define (split ls n)
  (define (head ls n)
    (cond
     [(null? ls) '()]
     [(zero? n) '()]
     [else
      (cons (car ls)
            (head (cdr ls) (1- n)))]))
  (define (tail ls n)
    (cond
     [(null? ls) '()]
     [(zero? n) ls]
     [else
      (tail (cdr ls) (1- n))]))
  (list (head ls n)
        (tail ls n)))

(test (split '(a b) 2) '((a b) ()))
(test (split '(a b) 1) '((a) (b)))
(test (split '(a b) 0) '(() (a b)))
(test (split '(a b c d e f g) 3) '((a b c) (d e f g)))

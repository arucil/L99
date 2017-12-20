(load "prelude.scm")

;;; P16

(define (drop ls n)
  (let f ([ls ls] [i n])
    (cond
     [(null? ls) '()]
     [(= 1 i)
      (f (cdr ls) n)]
     [else
      (cons (car ls)
            (f (cdr ls) (1- i)))])))

(test (drop '() 1) '())
(test (drop '(a) 1) '())
(test (drop '(a) 2) '(a))
(test (drop '(a b) 2) '(a))
(test (drop '(a b) 3) '(a b))
(test (drop '(a b c d e f g h i j) 3) '(a b d e g h j))

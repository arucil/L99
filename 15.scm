(load "prelude.scm")

;;; P15

(define (repli ls n)
  (let f ([ls ls] [i n])
    (cond
     [(null? ls) '()]
     [(zero? i)
      (f (cdr ls) n)]
     [else
      (cons (car ls)
            (f ls (1- i)))])))

(test (repli '() 1) '())
(test (repli '(a) 1) '(a))
(test (repli '(a) 3) '(a a a))
(test (repli '(a) 0) '())
(test (repli '(a b) 2) '(a a b b))
(test (repli '(a b c) 3) '(a a a b b b c c c))

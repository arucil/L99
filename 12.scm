(load "prelude.scm")

;;; P12

(define (decode ls)
  (letrec ([repeat
            (lambda (n x)
              (if (zero? n)
                  '()
                  (cons x (repeat (1- n) x))))]
           [f
            (lambda (ls)
              (cond
               [(null? ls) '()]
               [(pair? (car ls))
                (append (repeat (caar ls) (cadar ls))
                        (f (cdr ls)))]
               [else
                (cons (car ls)
                      (f (cdr ls)))]))])
    (f ls)))

(test (decode '()) '())
(test (decode '(a)) '(a))
(test (decode '((2 a))) '(a a))
(test (decode '((2 a) b)) '(a a b))
(test (decode '(a b)) '(a b))
(test (decode '((4 a) b (2 c) (2 a) d (4 e))) '(a a a a b c c a a d e e e e))

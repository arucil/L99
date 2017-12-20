(load "prelude.scm")

;;; P10 & P13

(define (encode ls)
  (if (null? ls)
      '()
      (let f ([ls (cdr ls)] [x (car ls)] [n 1])
        (cond
         [(null? ls)
          (list (list n x))]
         [(equal? (car ls) x)
          (f (cdr ls) x (1+ n))]
         [else
          (cons (list n x)
                (f (cdr ls) (car ls) 1))]))))

(test (encode '()) '())
(test (encode '(a)) '((1 a)))
(test (encode '(a a)) '((2 a)))
(test (encode '(a a b)) '((2 a) (1 b)))
(test (encode '(a a a a b c c a a d e e e e)) '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)))

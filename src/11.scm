(load "prelude.scm")

;;; P11

(define (encode-modified ls)
  (if (null? ls)
      '()
      (let f ([ls (cdr ls)] [x (car ls)] [n 1])
        (cond
         [(null? ls)
          (list (if (= 1 n) x (list n x)))]
         [(equal? (car ls) x)
          (f (cdr ls) x (1+ n))]
         [else
          (cons (if (= 1 n) x (list n x))
                (f (cdr ls) (car ls) 1))]))))

(test (encode-modified '()) '())
(test (encode-modified '(a)) '(a))
(test (encode-modified '(a a)) '((2 a)))
(test (encode-modified '(a a b)) '((2 a) b))
(test (encode-modified '(a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e)))

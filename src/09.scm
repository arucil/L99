(load "prelude.scm")

;;; P09

(define (pack ls)
  (if (null? ls)
      '()
      (let f ([ls (cdr ls)] [r (list (car ls))])
        (cond
         [(null? ls)
          (list r)]
         [(equal? (car ls) (car r))
          (f (cdr ls) (cons (car r) r))]
         [else
          (cons r (f (cdr ls) (list (car ls))))]))))

(test (pack '()) '())
(test (pack '(a)) '((a)))
(test (pack '(a a)) '((a a)))
(test (pack '(a a b)) '((a a) (b)))
(test (pack '(a a a a b c c a a d e e e e)) '((a a a a) (b) (c c) (a a) (d) (e e e e)))

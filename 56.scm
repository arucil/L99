(load "prelude.scm")

;;; P56

(define (reverse-tree t)
  (if (null? t)
      '()
      (list (car t)
            (reverse-tree (caddr t))
            (reverse-tree (cadr t)))))

(define (symmetric-tree? t)
  (equal? t (reverse-tree t)))

(test (symmetric-tree? '()) #t)
(test (symmetric-tree? '(x () ())) #t)
(test (symmetric-tree? '(x (x () ()) ())) #f)
(test (symmetric-tree? '(x (x () ()) (x () ()))) #t)
(test (symmetric-tree? '(x (x () (x () ())) (x (x () ()) ()))) #t)

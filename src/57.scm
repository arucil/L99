(load "prelude.scm")

;;; P57

(define (tree-insert t x)
  (cond
   [(null? t)
    (list x '() '())]
   [(< x (car t))
    (list (car t)
          (tree-insert (cadr t) x)
          (caddr t))]
   [(> x (car t))
    (list (car t)
          (cadr t)
          (tree-insert (caddr t) x))]
   [else t]))

(define (bin-search-tree ls)
  (let f ([t '()] [ls ls])
    (if (null? ls)
        t
        (f (tree-insert t (car ls))
           (cdr ls)))))

(test (bin-search-tree '())
      '())
(test (bin-search-tree '(1))
      '(1 () ()))
(test (bin-search-tree '(3 5))
      '(3 () (5 () ())))
(test (bin-search-tree '(3 5 2))
      '(3 (2 () ()) (5 () ())))
(test (bin-search-tree '(3 2 5 7 1))
      '(3 (2 (1 () ()) ())
          (5 () (7 () ()))))

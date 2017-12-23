;;; P88

(load "prelude.scm")
(load "87.scm")


(define (split g)
  (define (find-edges nodes g)
    (filter
     (lambda (edge)
       (or (memv (car edge) nodes)
           (memv (cadr edge) nodes)))
     (cadr g)))

  (let f ([g g])
    (if (null? (car g))
        '()
        (let* ([nodes (dfs g (caar g))]
               [edges (find-edges nodes g)])
          (cons (list nodes edges)
                (f (list (remove* nodes (car g))
                         (remove* edges (cadr g)))))))))

(test (split '((a b c d) ((a b) (a c))))
      '(((a b c) ((a b) (a c)))
        ((d) ())))
(test (split '((a b c d) ((a b) (c d))))
      '(((a b) ((a b)))
        ((c d) ((c d)))))
(test (split '((a b c d) ((a b) (a c) (b d))))
      '(((a b d c) ((a b) (a c) (b d)))))

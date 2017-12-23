;;; P87

(load "prelude.scm")
(load "80.scm")


(define (dfs g n)
  (let ([ls (list n)]
        [g (graph-ge->al g)])
    (let f ([n n])
      (let ([nodes (cadr (assv n g))])
        (for-each
         (lambda (node)
           (when (not (memv node ls))
             (set! ls (cons node ls))
             (f node)))
         nodes)))
    (reverse ls)))

(test (dfs '((a b c d) ((a b) (a d) (c d)))
           'a)
      '(a b d c))

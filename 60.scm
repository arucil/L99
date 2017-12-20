(load "prelude.scm")
(load "22.scm")
(load "59.scm")

;;; P60

(define (count-nodes t)
  (if (null? t)
      0
      (+ 1
         (count-nodes (cadr t))
         (count-nodes (caddr t)))))

(define min-nodes
  (let ([memo '((0 . 0) (1 . 1))])
    (lambda (h)
      (let ([y (assv h memo)])
        (if y
            (cdr y)
            (let ([y (+ 1
                        (min-nodes (sub1 h))
                        (min-nodes (- h 2)))])
              (set! memo (cons (cons h y) memo))
              y))))))

(test (min-nodes 1) 1)
(test (min-nodes 5) 12)
(test (min-nodes 6) 20)

(define (max-height n)
  (do ([i 0 (add1 i)])
      [(> (min-nodes i) n)
       (sub1 i)]))

(test (max-height 1) 1)
(test (max-height 3) 2)
(test (max-height 13) 5)

(define (hbal-tree-nodes n)
  (filter
   (lambda (x)
     (= n (count-nodes x)))
   (apply
    append
    (map
     hbal-trees
     (range (ceiling (log (add1 n) 2))
            (max-height n))))))

(test (hbal-tree-nodes 3)
      '((x (x () ()) (x () ()))))
(test (hbal-tree-nodes 4)
      '((x (x (x () ()) ()) (x () ()))
        (x (x () (x () ())) (x () ()))
        (x (x () ()) (x (x () ()) ()))
        (x (x () ()) (x () (x () ())))))

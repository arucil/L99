;;; P92

(load "prelude.scm")
(load "80.scm")
(load "22.scm")
(load "87.scm")


(define (number g)
  (define (new-edge-nums num g nodes)
    (map
     (lambda (neighbor)
       (let ([p (assv neighbor nodes)])
         (if p
             (abs (- num (cadr p)))
             0)))
     (cadar g)))

  (let f ([g (map (let ([g (graph-ge->al g)])
                    (lambda (node)
                      (assv node g)))
                  (dfs g (caar g)))]
          [numbered-nodes '()]
          [rest-nums (range 1 (length (car g)))]
          [edge-nums '()])
    (if (null? g)
        numbered-nodes
        (exists
         (lambda (num)
           (and (for-all
                 (lambda (neighbor)
                   (let ([p (assv neighbor numbered-nodes)])
                     (or (not p)
                         (not (memv (abs (- num (cadr p)))
                                    edge-nums)))))
                 (cadar g))
                (f (cdr g)
                   (cons (list (caar g) num)
                         numbered-nodes)
                   (remove num rest-nums)
                   (append (new-edge-nums num g numbered-nodes)
                           edge-nums))))
         rest-nums))))

(test (number '((a b c d e f g) ((a b) (a d) (a g) (b c) (b e) (e f))))
      '((g 7) (d 6) (f 4) (e 3) (c 2) (b 5) (a 1)))
(test (number '((a b c d e f g h i k m n p q) ((a b) (a c) (a h) (a i) (a g) (d c) (d k) (e q) (e c) (f c) (q m) (q n) (p n))))
      '((g 14) (i 13) (h 11) (f 5) (p 9) (n 7) (m 6) (q 10) (e 4) (k 8) (d 3) (c 12) (b 2) (a 1)))

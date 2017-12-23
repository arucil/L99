;;; P83

(load "prelude.scm")
(load "26.scm")

;;; assuming undirected graph

(define (s-tree g)
  (define (find-adj-edges edge edges rest-nodes)
    (filter
     (lambda (e)
       (and (or (memq (car e) rest-nodes)
                (memq (cadr e) rest-nodes))
            (or (eq? (car e) (car edge))
                (eq? (car e) (cadr edge))
                (eq? (cadr e) (car edge))
                (eq? (cadr e) (cadr edge)))))
     edges))
  (define (unique ls)
    (cond
     [(null? ls) '()]
     [(member (car ls) (cdr ls))
      (unique (cdr ls))]
     [else
      (cons (car ls)
            (unique (cdr ls)))]))

  (if (null? (cadr g))
      '()
      (letrec ([f
                (lambda (edge edges rest-nodes)
                  (if (null? rest-nodes)
                      (list (list edge))
                      (map
                       (lambda (edges)
                         (cons edge edges))
                       (apply append
                              (map
                               (lambda (e1)
                                 (f e1
                                    (remove e1 edges)
                                    (remove (car e1)
                                            (remove (cadr e1) rest-nodes))))
                               (find-adj-edges edge edges rest-nodes))))))])
        (map
         (lambda (edges)
           (list (car g) edges))
         (unique
          (apply append
                 (map
                  (lambda (edge)
                    (map (lambda (edges)
                           (sort
                            (lambda (e1 e2)
                              (let ([s1 (symbol->string (car e1))]
                                    [s2 (symbol->string (car e2))])
                                (if (string=? s1 s2)
                                    (string<? (symbol->string (cadr e1))
                                              (symbol->string (cadr e2)))
                                    (string<? s1 s2))))
                            edges))
                         (f edge
                            (remove edge (cadr g))
                            (remove (car edge)
                                    (remove (cadr edge) (car g))))))
                  (cadr g))))))))

(test (s-tree '((a b c)
                ((a b) (a c) (b c))))
      '(((a b c) ((a b) (a c)))
        ((a b c) ((a b) (b c)))
        ((a b c) ((a c) (b c)))))
(test (s-tree '((a b c d)
                ((a b) (a c) (b c) (c d))))
      '(((a b c d) ((a b) (a c) (c d)))
        ((a b c d) ((a b) (b c) (c d)))
        ((a b c d) ((a c) (b c) (c d)))))
(test (s-tree '((a b c d) ((a b) (a c) (a d))))
      '(((a b c d) ((a b) (a c) (a d)))))
(test (s-tree '((a b c d) ((a b) (a c) (b c))))
      '())



(define (tree? g)
  (= 1 (length (s-tree g))))


(define (connected? g)
  (> (length (s-tree g)) 0))

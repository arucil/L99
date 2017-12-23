;;; P80

(load "prelude.scm")


(define (digraph-ge->al g)
  (map
   (lambda (node)
     (list node
           (map
            (lambda (edge)
              (if (= (length edge) 2)
                  (cadr edge)
                  (cdr edge)))
            (assq* node (cadr g)))))
   (car g)))

(test (digraph-ge->al '((r s t u v) ((s r) (s u) (u r) (u s) (v u))))
      '((r ()) (s (r u)) (t ()) (u (r s)) (v (u))))
(test (digraph-ge->al '((k m p q) ((m q 7) (p m 5) (p q 9))))
      '((k ()) (m ((q 7))) (p ((m 5) (q 9))) (q ())))


(define (graph-ge->al g)
  (let ([edges (append
                (map
                 (lambda (edge)
                   (cons (cadr edge)
                         (cons (car edge)
                               (cddr edge))))
                 (cadr g))
                (cadr g))])
    (map
     (lambda (node)
       (list node
             (map
              (lambda (edge)
                (if (= (length edge) 2)
                    (cadr edge)
                    (cdr edge)))
              (assq* node edges))))
     (car g))))

(test (graph-ge->al '((b c d f g h k) ( (b c) (b f) (c f) (f k) (g h) )))
      '((b (c f))
        (c (b f))
        (d ())
        (f (b c k))
        (g (h))
        (h (g))
        (k (f))))


(define (digraph-al->ge g)
  (list (map car g)
        (apply append
               (map
                (lambda (node)
                  (map
                   (lambda (v)
                     (if (symbol? v)
                         (list (car node) v)
                         (cons (car node) v)))
                   (cadr node)))
                g))))

(test (digraph-al->ge '((r ()) (s (r u)) (t ()) (u (r s)) (v (u))))
      '((r s t u v) ((s r) (s u) (u r) (u s) (v u))))
(test (digraph-al->ge '((k ()) (m ((q 7))) (p ((m 5) (q 9))) (q ())))
      '((k m p q) ((m q 7) (p m 5) (p q 9))))


(define (graph-al->ge g)
  (define (unique ls)
    (cond
     [(null? ls) '()]
     [(member (car ls) (cdr ls))
      (unique (cdr ls))]
     [(member (cons (cadar ls)
                    (cons (caar ls)
                          (cddar ls)))
              (cdr ls))
      (unique (cdr ls))]
     [else
      (cons (car ls)
            (unique (cdr ls)))]))
  (list (map car g)
        (unique
         (apply append
                (map
                 (lambda (node)
                   (map
                    (lambda (v)
                      (if (symbol? v)
                          (list (car node) v)
                          (cons (car node) v)))
                    (cadr node)))
                 g)))))

(test (graph-al->ge '((r ()) (s (r u)) (t ()) (u (r s)) (v (u))))
      '((r s t u v) ((s r) (u r) (u s) (v u))))
(test (graph-al->ge '((k ()) (m ((q 7))) (p ((m 5) (q 9))) (q ())))
      '((k m p q) ((m q 7) (p m 5) (p q 9))))


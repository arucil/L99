;;; P81

(load "prelude.scm")

;; assuming undirected graph

(define (path g a b)
  (path-aux g a b '()))

(define (path-aux g a b passed)
  (let ([edges (append (cadr g)
                       (map
                        (lambda (edge)
                          (list (cadr edge) (car edge)))
                        (cadr g)))])
    (let f ([node a] [passed passed])
      (cond
       [(memq node passed)
        '()]
       [(eq? node b)
        (list (list b))]
       [else
        (map
         (lambda (path)
           (cons node path))
         (apply append
                (map
                 (lambda (edge)
                   (f (cadr edge) (cons node passed)))
                 (assq* node edges))))]))))

(test (path '((b c d f g h k) ((b c) (b f) (c f) (d h) (f k) (g h)))
            'k 'f)
      '((k f)))
(test (path '((b c d f g h k) ((b c) (b f) (c f) (d h) (f k) (g h)))
            'f 'b)
      '((f b)
        (f c b)))
(test (path '((b c d f g h k) ((b c) (b f) (c f) (d h) (f k) (g h)))
            'f 'd)
      '())

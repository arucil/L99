;;; P82

(load "prelude.scm")
(load "80.scm")
(load "81.scm")


;; assuming undirected graph

(define (cycle g a)
  (define (find-nodes node edges)
    (let f ([ls edges])
      (cond
       [(null? ls) '()]
       [(eq? node (caar ls))
        (cons (cadar ls)
              (f (cdr ls)))]
       [(eq? node (cadar ls))
        (cons (caar ls)
              (f (cdr ls)))]
       [else
        (f (cdr ls))])))
  (let f ([node a] [edges (cadr g)] [start #f])
    (cond
     [(null? edges)
      '()]
     [(and start (eq? node a))
      (list (list a))]
     [else
      (map
       (lambda (path)
         (cons node path))
       (apply append
              (map
               (lambda (next-node)
                 (f next-node
                    (remove (list node next-node)
                            (remove (list next-node node)
                                    edges))
                    #t))
               (find-nodes node edges))))])))

(test (cycle '((b c d f g h k) ((b c) (b f) (c f) (d h) (f k) (g h)))
             'f)
      '((f b c f)
        (f c b f)))
(test (cycle '((b c d f g h k) ((b c) (b f) (c f) (d h) (f k) (g h)))
             'k)
      '())

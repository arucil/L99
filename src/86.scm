;;; P86

(load "prelude.scm")


;; a

(define (degree g n)
  (let f ([es (cadr g)]
          [c 0])
    (if (null? es)
        c
        (f (cdr es)
           (+ c
              (if (eqv? n (caar es)) 1 0)
              (if (eqv? n (cadar es)) 1 0))))))

(test (degree '((a b c d) ((a b) (a c) (b c) (c d)))
              'a)
      2)
(test (degree '((a b c d) ((a b) (a c) (b c) (c d)))
              'c)
      3)
(test (degree '((a b c d) ((a b) (a c) (b c) (c d)))
              'd)
      1)


;; b

(define (degree-desc-nodes g)
  (sort
   (lambda (n1 n2)
     (> (degree g n1)
        (degree g n2)))
   (car g)))

(test (degree-desc-nodes '((a b c d) ((a b) (a c) (b c) (c d))))
      '(c a b d))


;; c

(define (color g)
  (define (find-not-adjacent n nodes)
    (filter
     (lambda (node)
       (and (not (member (list n node) (cadr g)))
            (not (member (list node n) (cadr g)))))
     nodes))

  (list (sort
         (lambda (n1 n2)
           (string<? (symbol->string (car n1))
                     (symbol->string (car n2))))
         (let f ([nodes (degree-desc-nodes g)]
                 [color 1]
                 [new-nodes '()])
           (if (null? nodes)
               new-nodes
               (let ([n* (cons (car nodes)
                               (find-not-adjacent
                                (car nodes)
                                (cdr nodes)))])
                 (f (remove* n* nodes)
                    (add1 color)
                    (append (map (lambda (n)
                                   (list n color))
                                 n*)
                            new-nodes))))))
        (cadr g)))

(test (color '((a b c d) ((a b) (a c) (c d))))
      '(((a 1) (b 2) (c 2) (d 1))
        ((a b) (a c) (c d))))
(test (color '((a b c d) ((a b) (a c) (b d) (c d))))
      '(((a 1) (b 2) (c 2) (d 1))
        ((a b) (a c) (b d) (c d))))
(test (color '((a b c d) ((a b) (a c) (b d) (c d) (a d))))
      '(((a 1) (b 3) (c 3) (d 2))
        ((a b) (a c) (b d) (c d) (a d))))
(test (color '((a b c d) ((a b) (a c) (b d) (c d) (a d) (b c))))
      '(((a 1) (b 2) (c 3) (d 4))
        ((a b) (a c) (b d) (c d) (a d) (b c))))

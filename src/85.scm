;;; P85

(load "prelude.scm")


(define (isomorphic? g1 g2)
  (define (remove-one x ls)
    (cond
     [(null? ls) '()]
     [(equal? x (car ls)) (cdr ls)]
     [else (cons (car ls) (remove-one x (cdr ls)))]))
  (define (same-edges? es1 es2)
    (cond
     [(null? es1) #t]
     [(member (car es1) es2)
      (same-edges? (cdr es1)
                   (remove-one (car es1) es2))]
     [else
      (let ([rev (cons (cadar es1)
                       (cons (caar es1)
                             (cddar es1)))])
        (if (member rev es2)
            (same-edges? (cdr es1)
                         (remove-one rev es2))
            #f))]))

  (and (= (length (car g1))
          (length (car g2)))
       (= (length (cadr g1))
          (length (cadr g2)))
       (exists
        (lambda (nodes)
          (let ([env (map cons nodes (car g1))])
            (same-edges? (cadr g1)
                         (map
                          (lambda (edge)
                            (cons (cdr (assv (car edge) env))
                                  (cons (cdr (assv (cadr edge) env))
                                        (cddr edge))))
                          (cadr g2)))))
        (permutations (car g2)))))


(define (permutations ls)
  (if (null? ls)
      '(())
      (apply
       append
       (map
        (lambda (x)
          (map
           (lambda (y)
             (cons x y))
           (permutations (remove x ls))))
        ls))))

(test (isomorphic? '((a b c) ((a b) (b c)))
                   '((a b c) ((a c) (b c))))
      #t)
(test (isomorphic? '((a b c d) ((a b) (c d)))
                   '((a b c d) ((a d) (c b))))
      #t)
(test (isomorphic? '((a b c d) ((a b) (b d)))
                   '((a b c d) ((a b) (c d))))
      #f)
(test (isomorphic? '((a b c d e f g h)
                     ((a b) (a c) (b d) (c d) (e f) (e g) (f h) (g h) (b f) (c g)))
                   '((a b c d e f g h)
                     ((a b) (a c) (b d) (c d) (e f) (e g) (f h) (g h) (c g) (d h))))
      #f)
(test (isomorphic? '((a b c d g h i j)
                     ((a g) (a h) (a i) (b g) (b h) (b j) (c g) (c i) (c j) (d h) (d i) (d j)))
                   '((1 2 3 4 5 6 7 8)
                     ((1 2) (2 3) (3 4) (1 4) (5 6) (7 8) (6 7) (5 8) (1 5) (2 6) (3 7) (4 8))))
      #t)

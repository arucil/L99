;;; P84

(load "prelude.scm")
(load "83.scm")


(define (ms-tree g)
  (define (min q ls)
    (if (null? ls)
        #f
        (let f ([min (car ls)]
                [ls (cdr ls)]
                [c (q (car ls))])
          (if (null? ls)
              min
              (let ([c2 (q (car ls))])
                (if (< c2 c)
                    (f (car ls) (cdr ls) c2)
                    (f min (cdr ls) c)))))))
  (define (weight g)
    (apply + (map caddr (cadr g))))
  (let ([m (min weight (s-tree g))])
    (list m (weight m))))

(test (ms-tree '((a b c d) ((a b 1) (a c 2) (b c 3) (c d 4))))
      '(((a b c d) ((a b 1) (a c 2) (c d 4))) 7))

(test (ms-tree '((a b c d e f) ((a b 1) (a d 4) (a e 3) (b d 4) (b e 2) (c e 4) (c f 5) (d e 4) (e f 7))))
      '(((a b c d e f) ((a b 1) (a d 4) (b e 2) (c e 4) (c f 5))) 16))

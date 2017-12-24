;;; P91

(load "prelude.scm")


(define (knight n x y)
  (define (jump set x y)
    (filter
     (lambda (p)
       (and (<= 1 (car p) n)
            (<= 1 (cadr p) n)
            (not (member p set))))
     (map
      (lambda (d)
        (list (+ x (car d))
              (+ y (cadr d))))
      '((+2 +1)
        (+1 +2)
        (+2 -1)
        (+1 -2)
        (-2 -1)
        (-1 -2)
        (-2 +1)
        (-1 +2)))))

  (let f ([set (list (list x y))] [x x] [y y])
    (if (= (* n n)
           (length set))
        set
        (let ([p* (jump set x y)])
          (if (null? p*)
              #f)
          (exists
           (lambda (p)
             (f (cons p set)
                (car p)
                (cadr p)))
           p*)))))

(test (knight 3 1 1)
      #f)
(test (knight 5 1 1)
      '((5 1) (4 3) (5 5) (3 4) (1 5) (2 3) (4 2) (2 1) (1 3) (2 5)
        (4 4) (5 2) (3 1) (1 2) (2 4) (4 5) (3 3) (5 4) (3 5) (1 4)
        (2 2) (4 1) (5 3) (3 2) (1 1)))

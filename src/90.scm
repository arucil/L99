;;; 90

(load "prelude.scm")


(define (queens n)
  (define mask (expt 2 n))
  (define all-bits (sub1 mask))
  (define bits-list
    (map (lambda (i)
           (expt 2 i))
         (iota n)))
  (define (done? row)
    (= row all-bits))
  (define (bits r)
    (let f ([n (expt 2 (sub1 n))]
            [r r])
      (cond
       [(zero? r) '()]
       [(>= r n)
        (cons n (f (/ n 2) (- r n)))]
       [else
        (f (/ n 2) r)])))
  (define (available-bits row l r)
    (remove* (bits row)
             (remove* (bits l)
                      (remove* (bits r) bits-list))))

  (let f ([row 0] [l 0] [r 0] [s '()])
    (if (done? row)
        (list s)
        (apply append
               (map
                (lambda (bit)
                  (f (+ row bit)
                     (remainder (* 2 (+ l bit)) mask)
                     (quotient (+ r bit) 2)
                     (cons (add1 (inexact->exact (log bit 2))) s)))
                (available-bits row l r))))))

(test (queens 8)
      '((4 2 7 3 6 8 5 1) (5 2 4 7 3 8 6 1) (3 5 2 8 6 4 7 1)
        (3 6 4 2 8 5 7 1) (5 7 1 3 8 6 4 2) (4 6 8 3 1 7 5 2)
        (3 6 8 1 4 7 5 2) (5 3 8 4 7 1 6 2) (5 7 4 1 3 8 6 2)
        (4 1 5 8 6 3 7 2) (3 6 4 1 8 5 7 2) (4 7 5 3 1 6 8 2)
        (6 4 2 8 5 7 1 3) (6 4 7 1 8 2 5 3) (1 7 4 6 8 2 5 3)
        (6 8 2 4 1 7 5 3) (6 2 7 1 4 8 5 3) (4 7 1 8 5 2 6 3)
        (5 8 4 1 7 2 6 3) (4 8 1 5 7 2 6 3) (2 7 5 8 1 4 6 3)
        (1 7 5 8 2 4 6 3) (2 5 7 4 1 8 6 3) (4 2 7 5 1 8 6 3)
        (5 7 1 4 2 8 6 3) (6 4 1 5 8 2 7 3) (5 1 4 6 8 2 7 3)
        (5 2 6 1 7 4 8 3) (6 3 7 2 8 5 1 4) (2 7 3 6 8 5 1 4)
        (7 3 1 6 8 5 2 4) (5 1 8 6 3 7 2 4) (1 5 8 6 3 7 2 4)
        (3 6 8 1 5 7 2 4) (6 3 1 7 5 8 2 4) (7 5 3 1 6 8 2 4)
        (7 3 8 2 5 1 6 4) (5 3 1 7 2 8 6 4) (2 5 7 1 3 8 6 4)
        (3 6 2 5 8 1 7 4) (6 1 5 2 8 3 7 4) (8 3 1 6 2 5 7 4)
        (2 8 6 1 3 5 7 4) (5 7 2 6 3 1 8 4) (3 6 2 7 5 1 8 4)
        (6 2 7 1 3 5 8 4) (3 7 2 8 6 4 1 5) (6 3 7 2 4 8 1 5)
        (4 2 7 3 6 8 1 5) (7 1 3 8 6 4 2 5) (1 6 8 3 7 4 2 5)
        (3 8 4 7 1 6 2 5) (6 3 7 4 1 8 2 5) (7 4 2 8 6 1 3 5)
        (4 6 8 2 7 1 3 5) (2 6 1 7 4 8 3 5) (2 4 6 8 3 1 7 5)
        (3 6 8 2 4 1 7 5) (6 3 1 8 4 2 7 5) (8 4 1 3 6 2 7 5)
        (4 8 1 3 6 2 7 5) (2 6 8 3 1 4 7 5) (7 2 6 3 1 4 8 5)
        (3 6 2 7 1 4 8 5) (4 7 3 8 2 5 1 6) (4 8 5 3 1 7 2 6)
        (3 5 8 4 1 7 2 6) (4 2 8 5 7 1 3 6) (5 7 2 4 8 1 3 6)
        (7 4 2 5 8 1 3 6) (8 2 4 1 7 5 3 6) (7 2 4 1 8 5 3 6)
        (5 1 8 4 2 7 3 6) (4 1 5 8 2 7 3 6) (5 2 8 1 4 7 3 6)
        (3 7 2 8 5 1 4 6) (3 1 7 5 8 2 4 6) (8 2 5 3 1 7 4 6)
        (3 5 2 8 1 7 4 6) (3 5 7 1 4 2 8 6) (5 2 4 6 8 3 1 7)
        (6 3 5 8 1 4 2 7) (5 8 4 1 3 6 2 7) (4 2 5 8 6 1 3 7)
        (4 6 1 5 2 8 3 7) (6 3 1 8 5 2 4 7) (5 3 1 6 8 2 4 7)
        (4 2 8 6 1 3 5 7) (6 3 5 7 1 4 2 8) (6 4 7 1 3 5 2 8)
        (4 7 5 2 6 1 3 8) (5 7 2 6 3 1 4 8)))
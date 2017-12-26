;;; P97

(load "prelude.scm")


(define (sudoku board)
  (let f ([state (make-state board)])
    (let ([pos (next-hole state)])
      (if pos
          (exists
           (lambda (num)
             (f (fill-hole state pos num)))
           (valid-nums state pos))
          (get-solution state)))))

(define (make-state board)
  board)

(define (get-solution state)
  state)

(define (next-hole state)
  (let f ([y 0] [rows state])
    (if (null? rows)
        #f
        (let ([x (index-of 0 (car rows))])
          (if (>= x 0)
              (cons x y)
              (f (add1 y) (cdr rows)))))))

(define (index-of x ls)
  (let f ([i 0] [ls ls])
    (cond
     [(null? ls) -1]
     [(equal? x (car ls)) i]
     [else (f (add1 i) (cdr ls))])))

(define (fill-hole state pos num)
  (list-subst (cdr pos)
              (list-subst (car pos)
                          num
                          (list-ref state (cdr pos)))
              state))

(define (list-subst i new ls)
  (cond
   [(null? ls) '()]
   [(zero? i)
    (cons new (cdr ls))]
   [else
    (cons (car ls)
          (list-subst (sub1 i)
                      new
                      (cdr ls)))]))

(define (valid-nums state pos)
  (remove* (square-nums state pos)
           (remove* (list-ref state (cdr pos))
                    (remove* (map (lambda (row)
                                    (list-ref row (car pos)))
                                  state)
                             '(1 2 3 4 5 6 7 8 9)))))

(define (square-nums state pos)
  (let ([x0 (* 3 (quotient (car pos) 3))]
        [y0 (* 3 (quotient (cdr pos) 3))])
    (let ([row1 (list-ref state y0)]
          [row2 (list-ref state (+ 1 y0))]
          [row3 (list-ref state (+ 2 y0))])
      (list (list-ref row1 x0)
            (list-ref row1 (+ 1 x0))
            (list-ref row1 (+ 2 x0))
            (list-ref row2 x0)
            (list-ref row2 (+ 1 x0))
            (list-ref row2 (+ 2 x0))
            (list-ref row3 x0)
            (list-ref row3 (+ 1 x0))
            (list-ref row3 (+ 2 x0))))))

(pretty-print
 (sudoku
  '((0 0 4 8 0 0 0 1 7)
    (6 7 0 9 0 0 0 0 0)
    (5 0 8 0 3 0 0 0 4)
    (3 0 0 7 4 0 1 0 0)
    (0 6 9 0 0 0 7 8 0)
    (0 0 1 0 6 9 0 0 5)
    (1 0 0 0 8 0 3 0 6)
    (0 0 0 0 0 6 0 9 1)
    (2 4 0 0 0 1 5 0 0))))

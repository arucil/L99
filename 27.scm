(load "prelude.scm")
(load "26.scm")

;;; P27

(define (remove x ls)
  (cond
   [(null? ls) '()]
   [(equal? x (car ls)) (cdr ls)]
   [else (cons (car ls)
               (remove x (cdr ls)))]))

(define (remove-xs xs ls)
  (if (null? xs)
      ls
      (remove-xs (cdr xs)
                 (remove (car xs) ls))))

(define (group ls group)
  (let f ([ls ls] [group group])
    (if (null? group)
        '(())
        (apply
         append
         (map
          (lambda (x)
            (map (lambda (y)
                   (cons x y))
                 (f (remove-xs x ls)
                    (cdr group))))
          (combination (car group) ls))))))

(test (group '(a) '(1)) '(((a))))
(test (group '(a b) '(2)) '(((a b))))
(test (group '(a b) '(1 1)) '(((a) (b)) ((b) (a))))
(test (group '(a b c) '(1 2)) '(((a) (b c)) ((b) (a c)) ((c) (a b))))

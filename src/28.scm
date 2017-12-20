(load "prelude.scm")
(load "17.scm")

;;; P28

(define (sort less? ls)
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (let* ([pair (split ls (quotient (length ls) 2))]
             [la (sort less? (car pair))]
             [lb (sort less? (cadr pair))])
        (let f ([la la] [lb lb])
          (cond
           [(null? la) lb]
           [(null? lb) la]
           [(less? (car la) (car lb))
            (cons (car la)
                  (f (cdr la) lb))]
           [else
            (cons (car lb)
                  (f la (cdr lb)))])))))

;; a

(define (lsort ls)
  (sort (lambda (a b)
          (<= (length a) (length b)))
        ls))

(test (lsort '(())) '(()))
(test (lsort '((a) ())) '(() (a)))
(test (lsort '((a) ())) '(() (a)))
(test (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o))) '((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l)))

;; b

(define (lfsort ls)
  (letrec ([count-len-freq
            (lambda (len lss)
              (cond
               [(null? lss) 0]
               [(= len (length (car lss)))
                (add1 (count-len-freq len (cdr lss)))]
               [else (count-len-freq len (cdr lss))]))])
    (sort (lambda (a b)
            (<= (count-len-freq (length a) ls)
                (count-len-freq (length b) ls)))
          ls)))

(test (lfsort '(())) '(()))
(test (lfsort '((a) ())) '((a) ()))
(test (lfsort '((a a) (a) (a b))) '((a) (a a) (a b)))
(test (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
      '((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n)))

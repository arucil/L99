(load "prelude.scm")

;;; P50

;; leaf: (freq symbol)
;; interior: (freq left right)

(define (huffman ls)
  (letrec ([min
            (lambda (ls)
              (let f ([ls (cdr ls)] [m (car ls)])
                (cond
                 [(null? ls) m]
                 [(< (caar ls) (car m)) (f (cdr ls) (car ls))]
                 [else (f (cdr ls) m)])))]
           [f
            (lambda (ls)
              (cond
               [(null? ls) '()]
               [(null? (cdr ls)) (car ls)]
               [else
                (let* ([m1 (min ls)]
                       [m2 (min (remove m1 ls))]
                       [ls (remove m2 (remove m1 ls))])
                  (f (cons (list (+ (car m1) (car m2))
                                 m1 m2)
                           ls)))]))]
           [code
            (lambda (t c)
              (cond
               [(null? (cddr t))
                (if (eq? c (cadr t))
                    ""
                    #f)] ; leaf
               [else
                (cond
                 [(code (cadr t) c)
                  (string-append "0" (code (cadr t) c))]
                 [(code (caddr t) c)
                  (string-append "1" (code (caddr t) c))]
                 [else #f])]))])
    (let ([t (f (map (lambda (p)
                       (list (cadr p) (car p)))
                     ls))])
      (map (lambda (p)
             (list (car p)
                   (code t (car p))))
           ls))))

(test (huffman '((A 1) (B 2))) '((A "0") (B "1")))
(test (huffman '((A 1) (B 2) (C 4))) '((A "00") (B "01") (C "1")))
(test (huffman '((A 45) (B 13) (C 12) (D 16) (E 9) (F 5))) '((A "0") (B "101") (C "100") (D "111") (E "1101") (F "1100")))

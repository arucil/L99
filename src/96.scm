;;; P96

(load "prelude.scm")


(define (identifier? str)
  (let ([ls (string->list str)])
    (and (first-char ls)
         (rest-chars (cdr ls)))))

(define (first-char ls)
  (and (not (null? ls))
       (char-alphabetic? (car ls))))

(define (rest-chars ls)
  (or (null? ls)
      (and (char=? #\- (car ls))
           (not (null? (cdr ls)))
           (rest-alnums (cdr ls)))
      (rest-alnums ls)))

(define (rest-alnums ls)
  (and (or (char-alphabetic? (car ls))
           (char-numeric? (car ls)))
       (rest-chars (cdr ls))))

(test (identifier? "A")
      #t)
(test (identifier? "a3")
      #t)
(test (identifier? "a3D")
      #t)
(test (identifier? "-a3D")
      #f)
(test (identifier? "a3D-")
      #f)
(test (identifier? "a3D-o2")
      #t)
(test (identifier? "a3D--o2")
      #f)
(test (identifier? "a3D-o2-37")
      #t)

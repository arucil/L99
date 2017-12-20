(load "prelude.scm")


;;; P06

(define (palindrome? ls)
  (equal? ls (reverse ls)))

(test (palindrome? '()) #t)
(test (palindrome? '(a)) #t)
(test (palindrome? '(a a)) #t)
(test (palindrome? '(a b)) #f)
(test (palindrome? '(a b a)) #t)
(test (palindrome? '(a b b)) #f)

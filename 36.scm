(load "prelude.scm")
(load "35.scm")
(load "13.scm")

;;; P36

(define (prime-factors-mult n)
  (map (lambda (x)
         (list (cadr x) (car x)))
       (encode (prime-factors n))))

(test (prime-factors-mult 3) '((3 1)))
(test (prime-factors-mult 16) '((2 4)))
(test (prime-factors-mult 63) '((3 2) (7 1)))

(load "prelude.scm")

;;; P21

(define (insert-at x ls n)
  (cond
   [(null? ls) (list x)]
   [(= 1 n) (cons x ls)]
   [else (cons (car ls)
               (insert-at x (cdr ls) (1- n)))]))

(test (insert-at 'x '() 1) '(x))
(test (insert-at 'x '(a) 1) '(x a))
(test (insert-at 'x '(a b c) 2) '(a x b c))
(test (insert-at 'x '(a b c) 3) '(a b x c))
(test (insert-at 'x '(a b c) 4) '(a b c x))

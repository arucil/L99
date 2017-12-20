(load "prelude.scm")

;;; P54A

(define (tree? t)
  (cond
   [(null? t) #t]
   [(and (list? t)
         (= 3 (length t)))
    (and (tree? (cadr t))
         (tree? (caddr t)))]
   [else #f]))

(test (tree? '()) #t)
(test (tree? '(a)) #f)
(test (tree? '(a (b () ()) ())) #t)
(test (tree? '(a (b () ()))) #f)

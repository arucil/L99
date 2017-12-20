;;; P69

(load "prelude.scm")

(define (dotstring t)
  (if (null? t)
      "."
      (string-append
       (symbol->string (car t))
       (dotstring (cadr t))
       (dotstring (caddr t)))))

(test (dotstring '())
      ".")
(test (dotstring '(a () ()))
      "a..")
(test (dotstring '(a (b (d () ())
                        (e () ()))
                     (c ()
                        (f (g () ())
                           ()))))
      "abd..e..c.fg...")

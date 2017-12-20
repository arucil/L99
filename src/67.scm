;;; P67

(load "prelude.scm")

;; a

(define (tree->string t)
  (cond
   [(null? t) ""]
   [(and (null? (cadr t))
         (null? (caddr t)))
    (symbol->string (car t))]
   [else
    (string-append
     (symbol->string (car t))
     "("
     (tree->string (cadr t))
     ","
     (tree->string (caddr t))
     ")")]))

(test (tree->string '(a (b (d () ()) (e () ()))
                        (c () (f (g () ()) ()))))
      "a(b(d,e),c(,f(g,)))")
(test (tree->string '())
      "")
(test (tree->string '(x () ()))
      "x")


(define (string->tree s)
  (let-values ([(t ls)
                (let f ([ls (string->list s)])
                  (if (or (null? ls)
                          (not (char-alphabetic? (car ls))))
                      (values '() ls)
                      (let ([x (string->symbol
                                (string (car ls)))])
                        (if (or (null? (cdr ls))
                                (not (char=? #\( (cadr ls))))
                            (values (list x '() '())
                                    (cdr ls))
                            (let-values ([(l ls)
                                          (f (cddr ls))])
                              (unless (char=? #\, (car ls))
                                (error 'string->tree "Invalid string" s))
                              (let-values ([(r ls)
                                            (f (cdr ls))])
                                (unless (char=? #\) (car ls))
                                  (error 'string->tree "Invalid string" s))
                                (values (list x l r)
                                        (cdr ls))))))))])
    t))

(test (string->tree "")
      '())
(test (string->tree "x")
      '(x () ()))
(test (string->tree "a(b(d,e),c(,f(g,)))")
      '(a (b (d () ()) (e () ()))
          (c () (f (g () ()) ()))))

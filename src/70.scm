;;; P70

(load "prelude.scm")


(define (string->tree s)
  (let-values
      ([(t ls)
        (let f ([ls (string->list s)])
          (define (children ls)
            (if (char=? #\^ (car ls))
                (values '() (cdr ls))
                (let-values ([(t ls) (f ls)])
                  (let-values ([(t* ls) (children ls)])
                    (values (cons t t*) ls)))))
          (let-values ([(x) (string->symbol
                             (string (car ls)))]
                       [(c* ls) (children (cdr ls))])
            (if (null? c*)
                (values x ls)
                (values (cons x c*) ls))))])
    t))

(test (string->tree "x^")
      'x)
(test (string->tree "ab^^")
      '(a b))
(test (string->tree "afg^^c^bd^e^^^")
      '(a (f g) c (b d e)))


(define (tree->string t)
  (if (symbol? t)
      (string-append
       (symbol->string t)
       "^")
      (string-append
       (symbol->string (car t))
       (apply string-append
              (map tree->string (cdr t)))
       "^")))

(test (tree->string 'x)
      "x^")
(test (tree->string '(a b))
      "ab^^")
(test (tree->string '(a (f g) c (b d e)))
      "afg^^c^bd^e^^^")

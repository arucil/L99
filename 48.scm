(load "prelude.scm")

;;; P48

(define (table-list lvar expr)
  (letrec ([eval
            (lambda (expr env)
              (cond
               [(pair? expr)
                (if (= 3 (length expr))
                    (let ([t1 (eval (car expr) env)]
                          [t2 (eval (caddr expr) env)])
                      (case (cadr expr)
                        [(and) (and t1 t2)]
                        [(or) (or t1 t2)]
                        [(nand) (not (and t1 t2))]
                        [(nor) (not (or t1 t2))]
                        [(xor) (or (and (not t1) t2)
                                   (and t1 (not t2)))]
                        [(impl) (or (not t1) t2)]
                        [(equ) (eq? t1 t2)]))
                    (let ([t1 (eval (cadr expr) env)])
                      (case (car expr)
                        [(not) (not t1)])))]
               [else
                (apply-env env expr)]))]
           [apply-env
            (lambda (env var)
              (cond
               [(null? env)
                (error 'table "variable not bound" var)]
               [(eq? (caar env) var) (cdar env)]
               [else (apply-env (cdr env) var)]))])
    (map
     (lambda (ls)
       (append ls
               (list (eval expr
                           (map cons lvar ls)))))
     (let f ([l lvar])
       (if (null? l)
           '(())
           (apply append
                  (map (lambda (x)
                         (list (cons #t x)
                               (cons #f x)))
                       (f (cdr l)))))))))

(test (table-list '(A) '(not A)) '((#t #f) (#f #t)))
(test (table-list '(A B) '(A and (A or (not B)))) '((#t #t #t)
                                                    (#f #t #f)
                                                    (#t #f #t)
                                                    (#f #f #f)))
(test (table-list '(A B C) '((A and (B or C)) equ ((A and B) or (A and C))))
      '((#t #t #t #t)
        (#f #t #t #t)
        (#t #f #t #t)
        (#f #f #t #t)
        (#t #t #f #t)
        (#f #t #f #t)
        (#t #f #f #t)
        (#f #f #f #t)))

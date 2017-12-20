(load "prelude.scm")

;;; P46

(define (table a b expr)
  (letrec ([truth
            (lambda (expr env)
              (cond
               [(pair? expr)
                (let ([t1 (truth (cadr expr) env)]
                      [t2 (truth (caddr expr) env)])
                  (case (car expr)
                    [(and) (and t1 t2)]
                    [(or) (or t1 t2)]
                    [(nand) (not (and t1 t2))]
                    [(nor) (not (or t1 t2))]
                    [(xor) (or (and (not t1) t2)
                               (and t1 (not t2)))]
                    [(impl) (or (not t1) t2)]
                    [(equ) (eq? t1 t2)]))]
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
     (lambda (p)
       (list (car p) (cadr p)
             (truth expr
                    (cons (cons a (car p))
                          (cons (cons b (cadr p))
                                '())))))
     '((#t #t)
       (#t #f)
       (#f #t)
       (#f #f)))))

(test (table 'A 'B '(and A (or A B))) '((#t #t #t)
                                        (#t #f #t)
                                        (#f #t #f)
                                        (#f #f #f)))

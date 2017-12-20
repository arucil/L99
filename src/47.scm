(load "prelude.scm")

;;; P47

(define (table-infix a b expr)
  (letrec ([truth
            (lambda (expr env)
              (cond
               [(pair? expr)
                (if (= 3 (length expr))
                    (let ([t1 (truth (car expr) env)]
                          [t2 (truth (caddr expr) env)])
                      (case (cadr expr)
                        [(and) (and t1 t2)]
                        [(or) (or t1 t2)]
                        [(nand) (not (and t1 t2))]
                        [(nor) (not (or t1 t2))]
                        [(xor) (or (and (not t1) t2)
                                   (and t1 (not t2)))]
                        [(impl) (or (not t1) t2)]
                        [(equ) (eq? t1 t2)]))
                    (let ([t1 (truth (cadr expr) env)])
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

(test (table-infix 'A 'B '(A and (A or (not B)))) '((#t #t #t)
                                                    (#t #f #t)
                                                    (#f #t #f)
                                                    (#f #f #f)))

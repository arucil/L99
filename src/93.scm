;;; P93

(load "prelude.scm")
(load "17.scm") ;; split


(define (equationize ls)
  (let ([n (length ls)])
    (if (< n 2)
        '()
        (unique (map equation->string (equations ls))))))

(define (eval e env)
  (let ([p (assv e env)])
    (if p
        (cdr p)
        (if (number? e)
            e
            (case (car e)
              [(+) (+ (eval (cadr e) env)
                      (eval (caddr e) env))]
              [(-) (- (eval (cadr e) env)
                      (eval (caddr e) env))]
              [(*) (* (eval (cadr e) env)
                      (eval (caddr e) env))]
              [(/) (/ (eval (cadr e) env)
                      (eval (caddr e) env))]
              [else (error 'eval "wtf" e)])))))

(define (equations ls)
  (do ([i 1
          (add1 i)]
       [result '()
               (let* ([parts (split ls i)]
                      [envl (map cons (iota i) (car parts))]
                      [envr (map cons (iota (- (length ls) i)) (cadr parts))]
                      [l* (expressions i)]
                      [r* (expressions (- (length ls) i))])
                 (apply append
                        result
                        (map
                         (lambda (l)
                           (filter
                            (lambda (x) x)
                            (map
                             (lambda (r)
                               (if (= (eval l envl)
                                      (eval r envr))
                                   (list '=
                                         (subst/env envl l)
                                         (subst/env envr r))
                                   #f))
                             r*)))
                         l*)))])
      [(= i (length ls))
       result]))

(define (unique ls)
  (cond
   [(null? ls) '()]
   [(member (car ls) (cdr ls))
    (unique (cdr ls))]
   [else
    (cons (car ls)
          (unique (cdr ls)))]))

(define (equation->string e)
  (let f ([e e]
          [prec 0])
    (if (number? e)
        (number->string e)
        (case (car e)
          [(+ -)
           (let ([s (string-append
                     (f (cadr e) 1)
                     (symbol->string (car e))
                     (f (caddr e) (if (eq? (car e) '-) 2 1)))])
             (if (> prec 1)
                 (string-append "(" s ")")
                 s))]
          [(* /)
           (let ([s (string-append
                     (f (cadr e) 3)
                     (symbol->string (car e))
                     (f (caddr e) (if (eq? (car e) '/) 4 3)))])
             (if (> prec 3)
                 (string-append "(" s ")")
                 s))]
          [(=)
           (string-append
            (f (cadr e) 0)
            "="
            (f (caddr e) 0))]))))


(define (subst/env env s)
  (cond
   [(null? s) '()]
   [(assoc s env) => cdr]
   [(pair? s)
    (cons (subst/env env (car s))
          (subst/env env (cdr s)))]
   [else s]))

;; [a, b)
(define (range a b)
  (map
   (lambda (x)
     (+ x a))
   (iota (- b a))))

;; returns a list of expressions
;; variables are always 0 .. n-1
(define expressions
  (let ([memo '((1 . (0))
                (2 . ((+ 0 1)
                      (- 0 1)
                      (* 0 1)
                      (/ 0 1))))])
    (lambda (n)
      (let ([m (assv n memo)])
        (if m
            (cdr m)
            (do ([i 0
                    (add1 i)]
                 [res '()
                      (append
                       (let ([before (range 0 i)]
                             [after (range (+ i 2) n)]
                             [exprs (expressions (sub1 n))]
                             [vars (iota (sub1 n))])
                         (apply append 
                                (map
                                 (lambda (subexpr)
                                   (subst/env (map
                                               cons
                                               vars
                                               (append
                                                before
                                                (list (subst/env
                                                       (list (cons 0 i)
                                                             (cons 1 (add1 i)))
                                                       subexpr))
                                                after))
                                              exprs))
                                 (expressions 2))))
                       res)])
                [(> i (- n 2)) res]))))))


(test (equationize '(1 2 3))
      '("1+2=3"))
(test (equationize '(1 2 3 4))
      '("1=2+3-4"
        "1-2=3-4"))
(test (equationize '(2 3 5 7 11))
      '("2=3-(5+7-11)"
        "2=3-(5+7)+11"
        "2=3-5-(7-11)"
        "2=3-5-7+11"
        "2=(3*5+7)/11"
        "2*(3-5)=7-11"
        "2-(3-(5+7))=11"
        "2-(3-5-7)=11"
        "2-(3-5)+7=11"
        "2-3+5+7=11"))

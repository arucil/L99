(load "prelude.scm")

;;; P55

(define (cbal-trees n)
  (letrec ([f
            (lambda (n)
              (cond
               [(zero? n) '(())]
               [(even? n)
                (append (combine (sub1 (/ n 2))
                                 (/ n 2))
                        (combine (/ n 2)
                                 (sub1 (/ n 2))))]
               [else
                (combine (quotient n 2)
                         (quotient n 2))]))]
           [combine
            (lambda (l r)
              (let ([l* (f l)]
                    [r* (f r)])
                (apply append
                       (map
                        (lambda (l)
                          (map
                           (lambda (r)
                             (list 'x l r))
                           r*))
                        l*))))])
    (f n)))

(test (cbal-trees 0)
      '(()))
(test (cbal-trees 1)
      '((x () ())))
(test (cbal-trees 2)
      '((x () (x () ()))
        (x (x () ()) ())))
(test (cbal-trees 3)
      '((x (x () ()) (x () ()))))
(test (cbal-trees 4)
      '((x (x () ()) (x () (x () ())))
        (x (x () ()) (x (x () ()) ()))
        (x (x () (x () ())) (x () ()))
        (x (x (x () ()) ()) (x () ()))))
(test (cbal-trees 7)
      '((x (x (x () ()) (x () ())) (x (x () ()) (x () ())))))

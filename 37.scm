(load "prelude.scm")
(load "36.scm")

;;; P37

(define (totient-phi-2 m)
  (apply *
         (map
          (lambda (p)
            (* (sub1 (car p))
               (expt (car p) (sub1 (cadr p)))))
          (prime-factors-mult m))))

(test (totient-phi-2 10) 4)
(test (totient-phi-2 13) 12)
(test (totient-phi-2 1) 1)

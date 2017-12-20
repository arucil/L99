(load "prelude.scm")
(load "33.scm")

;;; P34

(define (totient-phi m)
  (let f ([i 2])
    (cond
     [(>= i m) 1]
     [(coprime? i m) (add1 (f (add1 i)))]
     [else (f (add1 i))])))

(test (totient-phi 10) 4)
(test (totient-phi 1) 1)
(test (totient-phi 13) 12)

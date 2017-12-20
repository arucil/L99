(load "prelude.scm")

;;; P49

(define gray
  (let ([cache '()])
    (lambda (n)
      (cond
       [(zero? n) '("")]
       [(assv n cache) => cdr]
       [else
        (let ([x (gray (sub1 n))])
          (append (map (lambda (s)
                         (string-append "0" s))
                       x)
                  (map (lambda (s)
                         (string-append "1" s))
                       (reverse x))))]))))

(test (gray 0) '(""))
(test (gray 1) '("0" "1"))
(test (gray 2) '("00" "01" "11" "10"))
(test (gray 3) '("000" "001" "011" "010" "110" "111" "101" "100"))

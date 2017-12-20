;;; P73

(load "prelude.scm")


(define (tree-ptl t)
  (if (symbol? t)
      (list 't "(" t "," "[" "]" ")")
      (append
       (list 't "(" (car t) "," "[")
       (cdr
        (apply append
               (map (lambda (x)
                      (cons "," (tree-ptl x)))
                    (cdr t))))
       (list "]" ")"))))

(test (tree-ptl 'x)
      '(t "(" x "," "[" "]" ")"))
(test (tree-ptl '(a b c))
      '(t "(" a "," "[" t "(" b "," "[" "]" ")" "," t "(" c "," "["
          "]" ")" "]" ")"))


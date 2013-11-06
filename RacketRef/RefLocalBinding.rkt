#lang Racket

; 2.9 Local Binding: let, let*, letrec, ...

; (let ([id val-expr] ...) body ...+)
; (let proc-id ([id init-expr] ...) body ...+)

(let ([x 5]) x)

(let ([x 5])
    (let ([x 2]
          [y x])
      (list y x)))

(let fac ([n 10])
  (if (zero? n)
      1
      (* n (fac (sub1 n)))))
; 3628800

(letrec ([is-even? (lambda (n)
                     (or (zero? n)
                         (is-odd? (sub1 n))))]
         [is-odd? (lambda (n)
                    (and (not (zero? n))
                         (is-even? (sub1 n))))])
  (is-odd? 11))
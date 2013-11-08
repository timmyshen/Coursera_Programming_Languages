#lang Racket

; A more powerful example
;(define powers-of-two
;  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
;    (lambda () (f 2))))

; Higher order function that uses stream
(define (number-until stream tester)
  (letrec ([f (lambda (s ans)
                (let ([pr (s)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))

(define (stream-maker fn arg) ; fn takes two args
  (letrec ([f (lambda (x) ; helper f is a func that takes one argument
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))

(define ones (stream-maker (lambda (x y) 1) 1))
(define nats (stream-maker + 1))
(define powers-of-two (stream-maker * 2))
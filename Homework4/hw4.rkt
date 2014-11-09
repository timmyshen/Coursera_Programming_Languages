
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Problem 1. 
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; Problem 2.
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

; Problem 3.
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

; Problem 4.
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; A sample stream here
;(define ones
;  (lambda () (cons 1 ones)))

; Yet another sample stream
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; A more powerful example
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

; Higher order function that uses stream
(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))

; Problem 5.
(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0) (- 0 x) x)
                      (lambda () (f (+ x 1))))
                )])
    (lambda () (f 1))))

;(define (triple x)
;  (letrec ([y (+ x 2)]
;           [f (lambda (z) (+ z y w x))]
;           [w (+ x 7)])
;    (f -9)))

; Problem 6.
(define dan-then-dog
  (letrec ([f (lambda () (cons "dan.jpg" (lambda () (g))))]
           [g (lambda () (cons "dog.jpg" (lambda () (f))))])
    (lambda () (f))))

; Problem 7.
(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s)))
          (stream-add-zero (cdr (s))))))

; Problem 8.
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))


; Problem 9.
(define (vector-assoc v vec)
  (let ([vlen (vector-length vec)])
    (define (f idx)
      (if (>= idx vlen)
          #f
          (let ([pr (vector-ref vec idx)])
            (cond
              [(not (pair? pr)) (f (+ idx 1))]
              [(equal? v (car pr)) pr]
              [#t (f (+ idx 1))]))))
    (f 0)))


; Problem 10
(define (cached-assoc xs n)
  (letrec (
           [cache (make-vector n #f)]
           [cache-index 0]
           [f (lambda (v)
                (let ([pr (vector-assoc v cache)])
                  (if pr
                      pr
                      (let ([new-pr (assoc v xs)])
                        (vector-set! cache cache-index new-pr)
                        (set! cache-index (modulo (+ cache-index 1) n))
                        new-pr))))])
    f))





(define while-less 4)

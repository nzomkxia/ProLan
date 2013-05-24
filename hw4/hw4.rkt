
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;1
(define (sequence low high stride)
  (if ( > low high)
      null
     (cons low (sequence (+ low stride) high stride))))

;2
(define (string-append-map xs suffix)
  (map (lambda (str)
       (string-append str suffix))
       xs))

;3
(define (list-nth-mod xs n)
  (cond
    [(null? xs) (error "list-nth-mod: empty list")]
    [(< n 0) (error "list-nth-mod: negative number")]
    [else ((list-ref xs (remainder n (length xs))))]
           ))

;4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let
          ([stream (s)])
        (cons (car stream) (stream-for-n-steps (cdr s) (- n 1))))))
;5
(define funny-number-stream
  (letrec
      ([f (lambda (x) (if ( = x 5) (cons (- x) (lambda () f (+ x 1))) (cons x (lambda () f (+ x 1)))))])
     (lambda () (f 1))))

;6
(define dan-then-dog
  (letrec
      ([f (lambda (x) (if ( = x 0) (cons "dan.jpg" (lambda () f 1)) (cons "dog.jpg" (lambda () f 0))))])
    (lambda () (f 0))))

;7
(define (stream-add-zero s)
  (letrec
      ([f (s)])
    (lambda () (cons (cons 0 (car f)) (stream-add-zero (cdr f))))))

;8
(define (cycle-lists xs ys)
  (letrec
     ([join (lambda (count) (cons (list-nth-mod xs count) (list-nth-mod ys count)))]
      [f (lambda (x) (cons (join x) (lambda () f (+ x 1))))])
    (lambda () (f 0))))

;9
(define (vector-assoc v vec)
  (letrec
      ([vec-len (vector-length vec)]
       [f (lambda (pos) (cond [>= pos vec-len #f]
                              [not (pair? (vector-ref vec pos)) (f (+ pos 1))]
                              [#t (if (equal? (car (vector-ref vec pos)) v)
                                      (car (vector-ref vec pos)) (f (+ pos 1)))]))])
    (f 0)))

;10
;(define (cached-assoc xs n)
;(letrec

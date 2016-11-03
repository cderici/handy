#lang racket

(require (for-syntax (only-in racket/syntax format-id with-syntax*)
                     racket/stxparam
                     syntax/parse))

(provide define/unroll unroll)
;(provide (for-syntax define/unroll))

;; TODO : handle for loops

(define-syntax (unroll stx)
  (syntax-parse stx
    [(_ 0 fname:id (fvar:id ...) fbody:expr e:expr)
     #;(begin (displayln (format "~a" (identifier-binding fname))) #'e)
     #'e]
    [(_ n:number fname:id (fvar:id ...) fbody:expr e:expr)
     (syntax-parse #'e
       #:literals (let define define/unroll set!)
       [(define (fname var ...) body ...) #'e]
       [(define var body) #'e]
       [(set! var:id body) #'(set! var (unroll n fname (fvar ...) fbody body))]
       [(define/unroll n (fname var ...) body ...)
        #'(define (fname var ...)
            (unroll n fname (var ...) body body) ...)]
       [(let loop ((var arg-exp) ...) body)
        #'(letrec ([loop (lambda (var ...)
                           (unroll n loop (var ...) body body))])
            (loop arg-exp ...))]
       [(do ([i:id init:expr step:expr] ...)
          (stop?:expr finish:expr ...) body:expr ...)
        (with-syntax ([body #'(if stop?
                                  (begin finish ...)
                                  (begin body ... (doloop step ...)))])
          #'((letrec ([doloop (lambda (i ...)
                                (unroll n doloop (i ...) body body))])
            doloop) init ...))]
       #;[((~literal fname) r:expr ...)#'"same"]
       #;[(f:id r:expr ...) #'"different"]
       [(f:id r:expr ...)
        (if (eq? (syntax->datum #'fname)
                 (syntax->datum #'f))
            (with-syntax ([new-n (- (syntax->datum #'n) 1)])
              #'(let ((fvar r) ...) (unroll new-n fname (fvar ...) fbody fbody)))
            #'(f (unroll n fname (fvar ...) fbody r) ...))]
       [(f:expr r:expr ...)
        #'((unroll n fname (fvar ...) fbody f) (unroll n fname (fvar ...) fbody r) ...)]
       [_ #'e])]))


(define-syntax (define/unroll stx)
  (syntax-parse stx
    [(_ n:number (fname:id var:id ...) body ...+)
     #'(define (fname var ...)
         (unroll n fname (var ...) body body) ...)]))

#;(define/unroll 12 (fact n) (if (< n 1) 1
                              (* n (fact (- n 1)))))

#;(define/unroll 2 (fact n i) (if (< n 1) i
                                (fact (- n 1) (* n i))))

#;(define/unroll 2 (fib n) (if (<= n 2) 1
                             (+ (fib (- n 1))
                                (fib (- n 2)))))
#;(define/unroll 3 (sum n)
  (define/unroll 1 (helper x) (if (<= x 0) 0 (+ x (helper (- x 1)))))
  
  (let loop ([i (helper n)] [sum 0])
    (if (<= i 0)
        sum
        (loop (- i 1) (+ sum i)))))

#;(define/unroll 2 (mutative n)
  (define sum 0)
  (set! sum 2)
  sum)

#;(define/unroll 2 (hede n)
  (define sum 0)
  
  (do ((i 0 (add1 i)))
    ((>= i 5) sum)
    (set! sum (add1 sum)))
    )








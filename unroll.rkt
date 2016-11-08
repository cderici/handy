#lang racket

(provide define/unroll unroll)

(require (for-syntax (only-in racket/syntax format-id with-syntax*)
                     syntax/parse))

;; TODO : handle for loops

(define-syntax (unroll stx)
  (syntax-parse stx
    #:datum-literals (lift-this)
    [(_ 0 fname:id (fvar:id ...) fbody:expr e:expr) #'e]
    [(_ _ _ (_ ...) _ (lift-this e:expr)) #'(lift-this e)]
    [(_ n:number fname:id (fvar:id ...) fbody:expr e:expr)
     (syntax-parse #'e
       #:literals (let define define/unroll set! lambda begin cond do)
       [(define (fname var ...) body ...) #'e]
       [(define var body:expr) #'e]
       [(set! var:id body:expr) #'(set! var (unroll n fname (fvar ...) fbody body))]
       [(begin) #'(void)] ;; <- (otherwise errors "begin: empty form not allowed in: (begin)"
       [(lambda (var:id ...) body:expr ...) #'(lambda (var ...) (unroll n fname (fvar ...) fbody body) ...)]
       [(define/unroll n (fname var ...) body ...)
        #'(define (fname var ...)
            (unroll n fname (var ...) body body) ...)]
       [(let loop ((var arg-exp) ...) body)
        #'(letrec ([loop (lambda (var ...)
                           (unroll n loop (var ...) body body))])
            (loop arg-exp ...))]
       [(cond (condition truth:expr ...) ... (else els:expr))
        #'(cond ((unroll n fname (fvar ...) fbody condition) (unroll n fname (fvar ...) fbody truth) ...) ...
                (else (unroll n fname (fvar ...) fbody els)))]
       [(letlike ([newvar:id rhs:expr] ...) letbody ...)
        #'(letlike ([newvar (unroll n fname (fvar ...) fbody rhs)] ...)
                   (unroll n fname (fvar ...) fbody letbody) ...)]
       [(do ([i:id init:expr ... step:expr] ...)
          (stop?:expr finish:expr ...) body:expr ...)
        ;; we might have some uninitialized ids, let's find'em
        (define uninit-ids* (map (λ (s) (car (syntax->list s)))
                                 (filter (λ (s) (= 1 (length (syntax->list s))))
                                         (syntax->list #'((i init ...) ...)))))
        (with-syntax ([(uninit-ids ...) uninit-ids*]
                      [body #'(if stop?
                                  (begin (void) finish ...)
                                  (begin body ... (doloop step ...)))])
          #'((letrec ([doloop (lambda (i ...)
                                (unroll n doloop (i ...) body body))])
            doloop) init ... ... uninit-ids ...))]
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

#;(define/unroll 2 (let-lambda n)
  (let ([k (lambda (n) n)])
    (k 5)))

#;(define/unroll 2 (empty-begin n)  
  (if 1 2 (begin))
  )

(define/unroll 2 (foo)
  (define depth 0)
  (do ((j 0 (+ j 1))
       (depth (+ depth 1)))
    ((= j 36) #f)))



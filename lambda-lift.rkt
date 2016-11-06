#lang racket

(provide lift-this)

(require (for-syntax racket/syntax
                     syntax/parse
                     syntax/free-vars))

(require "unroll.rkt")

;; TODO
;; 1 - lift a single lambda with a free variable (figure out a way to modify the call sites)
;;     (map (lambda (x) k) '(1 2 3))


(define-syntax (lift-this stx)
  (syntax-parse stx
    #:literals (define lambda define/unroll unroll)
    [(_ (~and lam (lambda (arg:id ...) body ...)))
     (define exp (local-expand #'lam 'expression '()))
     (define frees (free-vars exp))
     
     (syntax-parse exp
       #:literals (#%plain-lambda)
       [(#%plain-lambda (larg ...) lbody)
        (with-syntax* ([(free-ids ...) frees]
                       [lifted-id (syntax-local-lift-expression
                                   #'(lambda (free-ids ...)
                                       (lambda (larg ...) lbody)))])
          #'(lifted-id free-ids ...))])]
    [(_ (~and def (define (f:id arg:id ...) body ...)))
     ;; expanding for free-vars (-it only accepts core forms-)
     (define exp (local-expand #'(letrec ([f (lambda (arg ...) body ...)]) f) 'expression '()))
     (define frees (free-vars exp))

     (syntax-parse exp
       #:literals (letrec-values)
       [(letrec-values (((b:id) (lambda (larg ...) lbody ...))) dummy:id)
        (with-syntax* ([(free-ids ...) frees]
                       [exp-to-be-lifted
                        #'(letrec ((g (lambda (larg ... free-ids ...)
                                        (let-syntax ([b (lambda (stx)
                                                          (syntax-parse stx
                                                            [_ #'(lambda (larg ...)
                                                                   (g larg ... free-ids ...))]))])
                                          lbody ...)))) g)]
                       [lifted_id (syntax-local-lift-expression #'exp-to-be-lifted)])
          #'(define-syntax (f styx)
              (syntax-parse styx
                [(_ actual-args:expr (... ...))
                 #'(lifted_id actual-args (... ...) free-ids ...)])))])]
    ;; heavy lifting
    [(_ (~and defunroll (define/unroll n:number (f:id arg:id ...) body ...)))
       #'(lift-this (define (f arg ...) (unroll n f (arg ...) body body )...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (top-func n) (* n n))

(define (func)
  (define k 2)
  ;; lambda
  ((lift-this (lambda (x) (+ (top-func x) x))) k)
  ((lift-this (lambda (x) (top-func k))) 3)

  (map (lift-this (lambda (x) (+ x k))) '(1 2 3))
  
  ;(lift-this (define (bar x) 1 k))
  ;(bar 2)

  ;(lift-this (define (foo n) (+ k (bar n))))
  ;(foo 3)
  
  ;; def
  ;(lift-this (define (fact n) (if (<= n 1) 1 (* k (fact (- n 1))))))

  ;; lift + unroll
  #;(lift-this (define/unroll 2 (fact n)
               (if (<= n 1) 1 (* n (fact (- n 1))))))
  #;(lift-this (define/unroll 2 (fact n) n))
  #;(fact 4)
)

(func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

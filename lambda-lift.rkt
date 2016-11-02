#lang racket



(require (for-syntax racket/syntax
                     racket
                     syntax/parse
                     syntax/free-vars
                     )
         (for-template racket/base))

(provide lift-this)

;; TODO
;; 1 - lift a single lambda with a free variable (figure out a way to modify the call sites)
;;     (map (lambda (x) k) '(1 2 3))


(define-syntax (lift-this stx)
  (syntax-parse stx
    #:literals (define lambda)
    [(_ (~and lam (lambda (arg:id ...) body ...)))
     (define exp (local-expand #'lam 'expression '()))
     (syntax-parse exp
       #:literals (#%plain-lambda)
       [(#%plain-lambda (larg ...) lbody)
        (define frees (free-vars #'lbody))
        (when (not (= (length frees) (length (syntax-e #'(arg ...)))))
          (error 'lift-this "cannot lift a lambda expression with a free variable"))
        (with-syntax ([lam-id (syntax-local-lift-expression #'lam)])
          #'lam)])]
    [(_ (~and def (define (f:id arg:id ...) body ...)))
     ;; expanding for free-vars (-it only accepts core forms-)
     (define frees (free-vars (local-expand #'(letrec ([f (lambda (arg ...) body ...)]) f) 'expression '())))
     
     (with-syntax* ([(free-ids ...) frees]
                    [lifted_id (syntax-local-lift-expression
                                #'(letrec ([g (lambda (arg ... free-ids ...)
                                                (let-syntax ([f (lambda (stx)
                                                                  (syntax-parse stx
                                                                    [(_ act-args (... ...)) #'(g act-args (... ...) free-ids ...)]))])
                                                  body ...))]) g))])
       #'(define-syntax (f styx)
           (syntax-parse styx
             [(_ actual-args:expr (... ...))
              #'(lifted_id actual-args (... ...) free-ids ...)])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (top-func n) (* n n))

(define (foo)
  (define k 5)

  ;; lambda
  ((lift-this (lambda (x) (+ (top-func x) x))) k)

  (lift-this (define (bar x) (+ x x)))
  (bar 5)
  
  ;; def
  (lift-this (define (fact n) (if (<= n 1) 1 (* k (fact (- n 1))))))
  (fact 4))


(foo)
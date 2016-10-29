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
    [(_ (define (f:id arg:id ...) body ...))
     (define exp (local-expand #'(lambda (arg ...) body ...) 'expression '()))
     (syntax-parse exp
       #:literals (#%plain-lambda)
       [(#%plain-lambda (exp-arg ...) exp-body)
        (define frees (free-vars #'exp-body))
        (let ((diff (- (length frees) (length (syntax-e #'(arg ...))))))
          (with-syntax* ([(f-ids-all ...) frees]
                         [f-id (syntax-local-lift-expression #'(lambda (f-ids-all ...) exp-body))]
                         [(f-ids ...) (if (zero? diff) '() (drop frees diff))])
            #'(define-syntax (f styx)
                (syntax-parse styx
                  [(_ actual-args:expr (... ...))
                   #'(f-id actual-args (... ...) f-ids ...)]))))])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (top-func n) (* n n))

(define (foo)
  (define k 5)

  ;; lambda
  ((lift-this (lambda (x) (+ (top-func x) x))) k)
  
  ;; def
  (lift-this (define (bar x) (+ x k)))
  (bar 4))

(foo)


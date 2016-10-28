#lang racket

(require (for-syntax racket/syntax
                     racket
                     syntax/parse
                     syntax/free-vars
                     )
         (for-template racket/base))

;; TODO
;; 1 - lift a single lambda with a free variable (figure out a way to modify the call sites)
;;     (map (lambda (x) k) '(1 2 3))
;; 2 - get rid of the "go-get-id" shenanigan, and just use the vars extracted
;;     by the "free-vars"
;;     (currently unable to use those because inspector <attached to the free-vars>
;;     disarms them, resulting them to be NOT free-identifier=? with the same ids inside the body)


(define-syntax (lift-this stx)

  (define (go-get-id formals s-id body-stx)
    ;; check if s-id is one of the formals
    (let ([m (memf (λ (formal) (equal? s-id (identifier-binding-symbol formal))) formals)])
      (if m (car m) ;; if so, just push the actual formal identifier
          ;; otherwise, dive into the body
          (syntax-parse body-stx
            [i:id (if (equal? s-id (identifier-binding-symbol #'i)) #'i #f)]
            [(f:expr e:expr ...)
             (or (go-get-id formals s-id #'f)
                 (go-get-id formals s-id #'(e ...)))]
            [_ #f]))))
  
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
        (define correct-free-ids (map (λ (f) (go-get-id (syntax-e #'(arg ...)) (syntax-e f) #'(body ...))) frees))
        (let ((diff (- (length frees) (length (syntax-e #'(arg ...))))))
          (with-syntax* ([ids correct-free-ids]
                         [f-id (syntax-local-lift-expression #'(lambda ids body ...))]
                         [(e-args ...) (if (zero? diff) '() (drop correct-free-ids diff))])
            ;(syntax-local-introduce
             #'(define-syntax (f styx)
                 (syntax-parse styx
                   [(_ actual-args:expr (... ...))
                     #'(f-id actual-args (... ...) e-args ...)]))))])]))

(define (top-func n) (* n n))

(define (foo)
  (define k 5)

  ((lift-this (lambda (x) (+ (top-func x) x))) k)
  
  (lift-this (define (bar x) (+ x k)))
  (bar 4))

(foo)
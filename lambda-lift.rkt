#lang racket

(require (for-syntax racket/syntax
                     racket
                     syntax/parse
                     syntax/free-vars
                     )
         (for-template racket/base))

;; TODO
;; 1 - generalize over the "lambda" case (get rid of it if possible)
;; 2 - get rid of define/lift "turn a lifted def into a macro that'll
;;     a) lift its definition (with possible extra arguments)
;;     b) annotate its call sites with the extra args [this'll also gets rid of the hash table shenanigan]
;; 3 - figure out this disarm identifier thing,
;;     to get rid of this "go-get-id" and just use the vars
;;     extracted by the free-vars
;;     (currently unable to use those because inspector disarms them, resulting
;;     them to be NOT bound-identifier=? with the same ids inside the body)


(begin-for-syntax
  (define lifted-funcs-args (make-hash)))

(define-syntax (define/lift stx)

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
    #:datum-literals (lift-this)
    ;; (define/lift .... entry
    [(_ (func:id arg:id ...) body ...+)
     #'(define (func arg ...) (define/lift body) ...)]
    ;; lift lambda with immediate application
    [(_ (lift-this ((~and lam (lambda (larg ...) lbody ...)) act-args ...)))
     (define lam-expanded (local-expand #'lam 'expression '()))
     (define original-arg-num (length (syntax-e #'(larg ...))))
     (syntax-parse lam-expanded
       #:literals (#%plain-lambda)
       [(#%plain-lambda (exp-arg ...) exp-body)
        (define frees (free-vars #'exp-body))
        (define correct-free-ids (map (λ (f) (go-get-id (syntax-e #'(larg ...)) (syntax-e f) #'(lbody ...))) frees))
        (let ((diff (- (length frees) (length (syntax-e #'(larg ...))))))
          (if (zero? diff)
              (with-syntax ([liftid (syntax-local-lift-expression #'(lambda (larg ...) lbody ...))])
                #'(liftid act-args ...))
              (let ((extra-args (drop correct-free-ids diff)))
                (with-syntax* ([(e-args ...) extra-args]
                               [liftid (syntax-local-lift-expression #'(lambda (larg ... e-args ...) lbody ...))])
                  #'(liftid act-args ... e-args ...)))))])]
    ;; (lift-this (lambda (x) (+ x k)))
    [(_ (lift-this (~and lam (lambda (larg ...) lbody ...))))
     (define lam-expanded (local-expand #'lam 'expression '()))
     (syntax-parse lam-expanded
       #:literals (#%plain-lambda)
       [(#%plain-lambda (exp-arg ...) exp-body)
        (define frees (free-vars #'exp-body))
        (define correct-free-ids (map (λ (f) (go-get-id (syntax-e #'(larg ...)) (syntax-e f) #'(lbody ...))) frees))
        (syntax-local-lift-expression
         (with-syntax ([ids correct-free-ids])
           #'(lambda ids lbody ...)))])]
    ;; (lift-this (define (func2 x) (+ x x)))
    [(_ (lift-this (define (f:id var:id ...) body ...)))
     (when (hash-has-key? lifted-funcs-args #'f)
       (error 'define/lift "duplicate lifts"))
     ;; disect the body for free ids (too lazy to communicate it from lam case)
     (define lam-expanded (local-expand #'(lambda (var ...) body ...)
                                        'expression '()))
     (syntax-parse lam-expanded
       #:literals (#%plain-lambda)
       [(#%plain-lambda (exp-arg ...) exp-body)
        (define frees (free-vars #'exp-body))
        ;; register to the hash
        (hash-set! lifted-funcs-args (identifier-binding-symbol #'f) frees)
        (with-syntax ([lifted-id #'(define/lift (lift-this (lambda (var ...) body ...)))])
          #'(define f lifted-id))])]
    ;; application
    [(_ (f:id r:expr ...))
     ;(displayln lifted-funcs-args)
     (if (hash-has-key? lifted-funcs-args (identifier-binding-symbol #'f))
         (let* ([args (hash-ref lifted-funcs-args (identifier-binding-symbol #'f))]
                [num-args (length args)]
                [args-call-site (syntax-e #'(r ...))]
                [num-args-call-site (length args-call-site)])
           (if (= num-args num-args-call-site)
               #'(f r ...)
               (let ([extra-args (drop args num-args-call-site)])
                   (with-syntax ([(e-args ...) extra-args])
                     #'(f r ... e-args ...)))))
         #'(f r ...))]
    ;; application
    [(_ (f:expr r:expr ...))
     ;(displayln #'(r ...))
     #'((define/lift f) (define/lift r) ...)]
    ;; default
    [(_ e:expr) #'e]))



(define/lift (foo)
  
  (define k 5)
  ;(lift-this (lift-this (lambda (x) (+ x k)) 3))
  ;(lift-this ((lambda (x) (+ x ((lambda (k) ((lambda () k))) 4) k)) 3))
  
  (lift-this (define (bar x) (+ x k)))
  (bar 4)
  )

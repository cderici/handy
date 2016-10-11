#lang racket

(require json)

(provide json->sexp
         json-file->sexp
         #;count-nodes
         sexp->file
         json-file->composition
         simplify-json)

;; main func => simplify-json

(define compute-composition
  ;; counts ast node types
  (let ([composition (make-hash)])
    (λ (ast*)
      (if (list? ast*)
          (map compute-composition ast*)
          (if (hash? ast*)
              (hash-for-each ast*
                             (λ (key val)
                               (begin
                                 (if (hash-has-key? composition key)
                                     (hash-update! composition key add1)
                                     (hash-set! composition key 1))
                                 (if (hash? val)
                                     (compute-composition val)
                                     (if (list? val)
                                         (map compute-composition val)
                                         'dummy)))))
              'dummy))
      (sort (hash->list composition) (λ (v1 v2) (string<? (symbol->string (car v1))
                                                          (symbol->string (car v2))))))))

(define (json->sexp ast*)
  (hash-map ast*
            (λ (key val)
              (letrec ([val->sexp
                        (λ (val)
                          (let ([v* (cond
                                      [(hash? val) (json->sexp val)]
                                      [(list? val) (map val->sexp val)]
                                      [(pair? val) `(,(val->sexp (car val))
                                                     ,(val->sexp (cdr val)))]
                                      [else val])])
                            (cond
                              [(and (list? v*) (= (length v*) 0)) '()]
                              [(and (list? v*) (= (length v*) 1))
                               (car v*)]
                              [else v*])))])
                (match key
                  ['module `(module-info)]
                  ['source `(source-info)]
                  ['span `(__span__)]
                  ['position `(__position__)]
                  [(or 'operator 'source-name 'integer 'number)
                   (let ([v (val->sexp val)])
                     (if (and (list? v) (= (length v) 1))
                         (car v) v))]
                  ['lexical `(lex ,(val->sexp val))]
                  ['toplevel `(top ,(val->sexp val))]
                  [else
                   `(,key
                     ,(val->sexp val))])))))

(define (trim-down-ast-sexp ast)
  (let* ([top-body-forms 
          (car ;; (())
           (filter (λ (a-node) (eq? (car a-node) 'body-forms)) ast))]
         [forms** (cdr top-body-forms)]
         [forms* (cdar forms**)] ;; the first is configure-runtime
         ;; filter the benchmark stuff
         [benchs '(boyer-iters browse-iters cpstak-iters ctak-iters dderiv-iters deriv-iters destruc-iters diviter-iters divrec-iters puzzle-iters tak-iters
                               takl-iters trav1-iters trav2-iters triangl-iters ack-iters array1-iters cat-iters string-iters sum1-iters sumloop-iters
                               tail-iters wc-iters fft-iters fib-iters fibfp-iters mbrot-iters nucleic-iters pnpoly-iters sum-iters sumfp-iters
                               conform-iters dynamic-iters earley-iters fibc-iters graphs-iters lattice-iters matrix-iters maze-iters mazefun-iters
                               nqueens-iters paraffins-iters peval-iters pi-iters primes-iters ray-iters scheme-iters simplex-iters slatex-iters perm9-iters
                               nboyer-iters sboyer-iters gcbench-iters parsing-iters gcold-iters quicksort-iters
                               ;; benchmark boilerplates
                               main run-bench run-benchmark fatal-error call-with-output-file/truncate open-output-file/truncate)]
         [forms (filter (λ (def-val)
                          (match def-val
                            [`((define-values-body ,body)
                               (define-values-names ,name1)
                               (define-values ,name2)) (begin
                                                         (when (not (equal? name1 name2))
                                                           (displayln (format "WARNING: ~a not eq? ~a" name1 name2)))
                                                         (not (memv (string->symbol name1) benchs))
                                                         #;(or (string=? name1 "run") (string=? name1 "tak")))]
                            [else false]
                            #;[`(require ,_) false]
                            #;[`(,op call-with-values) false]
                            #;[else (error 'trim-down-ast-sexp "Unusual body-form : ~a" def-val)])) forms*)]
         )
    forms
    #;(cons 'body-forms forms)))


(define (get-json fname)
  (read-json (open-input-file fname)))

(define (json-file->sexp js-filename)
  (json->sexp (get-json js-filename)))

(define (json-file->composition js-filename)
  (map (λ (h) (displayln (format "~a ~a" (car h) (cdr h))))
       (compute-composition (get-json js-filename))))

(define (sexp->file sexp outfilepath)
  (with-output-to-file outfilepath
    (λ () (pretty-write sexp))
    #:exists 'replace))

(define (simplify-json json-path-str new-path-str)
  (sexp->file (trim-down-ast-sexp (json-file->sexp json-path-str)) new-path-str))

(define benchmarks '(string
                     sum sumfp sumloop sumrec sumrecfp sumtail
                     tak ctak cpstak takl
                     fib fibfp fibc fft
                     diviter divrec 
                     mbrot triangl array1 pnpoly nucleic pi gcbench deriv ack paraffins primes nqueens earley))

(define MODES '(regular byte bni))

(define (simplify-all)
  (for ([bench benchmarks])
    (for ([mode MODES])
      (let* ([ast-dir "../asts/"]
             [file-name (format "~a~a-~a.json" ast-dir bench mode)]
             [new-file-name (format "~a~a-~a.run" ast-dir bench mode)])
        (simplify-json file-name new-file-name)))))

#|

GRAPHVIZ STUFF

(define counts (make-hash))

(define (get-recent key)
  (if (hash-has-key? counts key)
      (hash-ref counts key)
      (begin (hash-set! counts key (format "~a_" key)) (format "~a_" key))))

(define (increment key)
  (if (hash-has-key? counts key)
      (let ([new-key (string-append (hash-ref counts key) "_")])
        (begin (hash-set! counts key new-key) new-key))
      (begin (hash-set! counts key (format "~a_" key)) (format "~a_" key))))

(define (count-nodes ast)
  (cond
    [(not (or (list? ast) (pair? ast))) 1]
    [(empty? ast) 0]
    [else (add1 (apply + (map count-nodes ast)))]))

(define (make-graph vertices)
  (make-hash (map (lambda (v) (cons v (set))) vertices)))

(define (add-node graph v)
  (hash-set! graph v (set)))

(define (add-edge graph u v)
  #;(hash-set! graph u (set-add (hash-ref graph u (set)) v))
  (hash-set! graph v (set-add (hash-ref graph v (set)) u)))


(define gr (make-graph '()))
;; this global graph setting is ugly!
(define (sexp->graph ast)
  (begin
    (set! gr (make-graph '()))
    (sexp->graph-worker ast #f)))

(define (sexp->graph-worker ast parent)
  (cond
    [(not (or (list? ast) (pair? ast))) ;; value (leaf)
     (if (not parent)
         (error 'sexp->graph "we have a value without a parent : ~a" ast)
         (let ([val (format "\"~a\"" (get-recent ast))])
           (begin
             (add-node gr val)
             (increment ast)
             (add-edge gr parent val))))]
    [(and (list? ast) (not (null? ast)) (symbol? (car ast)))
     (let ([op (format "\"~a\"" (get-recent (car ast)))])
       (begin
         (add-node gr op)
         (increment (car ast))
         (add-edge gr parent op)
         (map (λ (child) (sexp->graph-worker child op)) (cdr ast))))]
    [(list? ast)
     (let ([head (format "\"~a\"" (get-recent "{}"))])
       (begin
         (add-node gr head)
         (increment "list")
         (add-edge gr parent head)
         (map (λ (child) (sexp->graph-worker child head)) ast)))]
    [else
     (error 'sexp->graph "we have an unhandled case : ~a" ast)]))
       

(define (adjacent graph u)
  (hash-ref graph u))

(define (vertices graph)
  (hash-keys graph))

(define (print-dot graph file-name)
  (call-with-output-file file-name #:exists 'replace
    (lambda (out-file)
      (write-string "digraph {" out-file) (newline out-file)

      (for ([v (vertices graph)])
        (write-string (format "~a;\n" v) out-file))

      (for ([v (vertices graph)])
        (for ([u (adjacent graph v)])
          (write-string (format "~a:se -> ~a:nw;\n" u v) out-file)))

      (write-string "}" out-file)
      (newline out-file))))


(define g (make-graph (list "caner" "ali" "veli" "kemal")))

(add-edge g "kemal" "ali")
(add-edge g "veli" "kemal")
(add-edge g "veli" "caner")

(print-dot g "caner-graph")


|#

#lang racket

(require compiler/zo-parse
         setup/dirs
         ;racket/cmdline
         json
         #;"json-sexp.rkt")

(require "pycket/pycket/pycket-lang/zo-expand.rkt")
(require "pycket/pycket/pycket-lang/expand.rkt") ;; global-config - hash*

(define args (current-command-line-arguments))

(define modName "gcbench-bni")  ;#s((toplevel expr 0 form 0 zo 0) 7 8 #t #f)
(define module-name modName)
(define DEBUG false)

(define BENCH-ASTS false)

(define GENERATE-AST false)

;'(#s((varref expr 0 form 0 zo 0) #s((toplevel expr 0 form 0 zo 0) 9 68 #f #f) #f))

#;(define splice-compiled (zo-parse
                         (open-input-bytes
                          (with-output-to-bytes
                           (λ _ (write (compile #'(begin 3))))))))

(define forms-we-got (make-hash))

;(define forms-we-got (make-hash '((list . 0))))

(define (+! key)
  (if (hash-has-key? forms-we-got key)
      (hash-set! forms-we-got key (add1 (hash-ref forms-we-got key)))
      (hash-set! forms-we-got key 0)))

;; counts
(define (analyze-body body-form closure-refs)
  (cond
    ((list? body-form) (begin (when DEBUG (displayln "- List ")) (+! 'list) (map (λ (b) (analyze-body b closure-refs)) body-form)))
    ((boolean? body-form) (begin (when DEBUG (displayln "- Boolean ")) (+! 'boolean) 0))
    ((number? body-form) (begin (when DEBUG (displayln "Number ")) (+! 'number) 0))
    ((string? body-form) (begin (when DEBUG (displayln "String ")) (+! 'string) 0))
    ((symbol? body-form) (begin (when DEBUG (displayln "Symbol ")) (+! 'symbol) 0))
    ((with-cont-mark? body-form) (begin (when DEBUG (displayln "with-cont-mark ")) (+! 'with-cont-mark) (analyze-body (with-cont-mark-body body-form) closure-refs)))
    ((with-immed-mark? body-form) (begin (when DEBUG (displayln "with-immed-mark ")) (+! 'with-immed-mark) (analyze-body (with-immed-mark-body body-form) closure-refs)))
    ((boxenv? body-form) (begin (when DEBUG (displayln "boxenv ")) (+! 'boxenv) (analyze-body (boxenv-body body-form) closure-refs)))
    ((let-one? body-form) (begin (when DEBUG (displayln "let-one ")) (+! 'let-one) (+ (analyze-body (let-one-rhs body-form) closure-refs) (analyze-body (let-one-body body-form) closure-refs))))
    ((let-void? body-form) (begin (when DEBUG (displayln "let-void ")) (+! 'let-void) (analyze-body (let-void-body body-form) closure-refs)))
    ((case-lam? body-form) (begin (when DEBUG (displayln "case-lam ")) (+! 'case-lam) (apply + (map (λ (b) (analyze-body b closure-refs)) (case-lam-clauses body-form)))))
    ((install-value? body-form) (begin (when DEBUG (displayln "install-value ")) (+! 'install-value) (+ (analyze-body (install-value-rhs body-form) closure-refs)
                                                                                                        (analyze-body (install-value-body body-form) closure-refs))))
    ((module-variable? body-form) (begin (when DEBUG (displayln "module-variable ")) (+! 'module-variable) 0))
    ((primval? body-form) (begin (when DEBUG (displayln "primval ")) (+! 'primval) 0))
    ((application? body-form) (begin (when DEBUG (displayln "application ")) (+! 'application) (+ (analyze-body (application-rator body-form) closure-refs)
                                                                                                  (apply + (map (λ (b) (analyze-body b closure-refs)) (application-rands body-form))))))
    ((def-values? body-form) (begin (when DEBUG (displayln "def-values ")) (+! 'def-values) (analyze-body (def-values-rhs body-form) closure-refs)))
    ((seq? body-form) (begin (when DEBUG (displayln "seq ")) (+! 'seq) (apply + (map (λ (b) (analyze-body b closure-refs)) (seq-forms body-form)))))
    ((splice? body-form) (begin (when DEBUG (displayln "splice ")) (+! 'splice) (apply + (map (λ (b) (analyze-body b closure-refs)) (splice-forms body-form)))))
    ((assign? body-form) (begin (when DEBUG (displayln "SET! ")) (+! 'assign) (analyze-body (assign-rhs body-form) closure-refs)))
    ((branch? body-form) (begin (when DEBUG (displayln "branch ")) (+! 'branch) (+ (analyze-body (branch-test body-form) closure-refs)
                                                                                   (analyze-body (branch-then body-form) closure-refs)
                                                                                   (analyze-body (branch-else body-form) closure-refs))))
    ((apply-values? body-form) (begin (when DEBUG (displayln "apply-values ")) (+! 'apply-values) (+ (analyze-body (apply-values-proc body-form) closure-refs)
                                                                                                     (analyze-body (apply-values-args-expr body-form) closure-refs))))
    ((localref? body-form) (begin (when DEBUG (displayln  "localref ")) (+! 'localref) 0))
    ((lam? body-form) (begin (when DEBUG (displayln "lam ")) (+! 'lam) (analyze-body (lam-body body-form) closure-refs)))
    ((inline-variant? body-form) (begin (when DEBUG (displayln "inline-variant ")) (+! 'inline-variant) (analyze-body (inline-variant-inline body-form) closure-refs)))
    ((closure? body-form) (begin (when DEBUG (displayln "closure ")) (+! 'closure) (if (memv (closure-gen-id body-form) closure-refs) 0 (analyze-body (closure-code body-form)
                                                                                                                                                      (cons (closure-gen-id body-form) closure-refs)))))
    ((toplevel? body-form) (begin (when DEBUG (displayln "toplevel ")) (+! 'toplevel) 0))
    ((hash? body-form) (begin (when DEBUG (displayln  "Already hashed Val (pushed by let-one)")) (+! 'hash) 0))
    (error 'analyze-body "you missed something!!")))


(define depFile (read (open-input-file (string-append "compiled/" modName "_rkt.dep"))))
(define version (car depFile))

(define pycketDir (path->string (current-directory))) ;; MUST BE RUN UNDER PYCKET DIR
;; config
;; global-config
  
;; language          (language . ("/home/caner/programs/racket/collects/racket/main.rkt"))
;; langDep -> '(collects #"racket" #"main.rkt")
(define collectsDir (path->string (find-collects-dir)))
  
;; module-name          (module-name . "canerStr")
;; body-forms

(define comp-top (zo-parse (open-input-file (string-append "compiled/" modName "_rkt.zo"))))

;; (struct compilation-top zo (max-let-depth prefix code)
;; (struct prefix zo (num-lifts toplevels stxs)
;; (struct	module-variable zo (modidx sym pos phase constantness)

;; toplevels : #f | global-bucket | module-variable

(define code (compilation-top-code comp-top))

;(analyze-body (mod-body code) '())

;forms-we-got

;; (struct mod form (name
;;  	 	     srcname
;;  	 	     self-modidx
;;  	 	     prefix
;;  	 	     provides
;;  	 	     requires
;;  	 	     body
;;  	 	     syntax-bodies
;;  	 	     unexported
;;  	 	     max-let-depth
;;  	 	     dummy
;;  	 	     lang-info
;;  	 	     internal-context
;;  	 	     flags
;;  	 	     pre-submodules
;;  	 	     post-submodules)
;;
;;     #:extra-constructor-name make-mod
;;     #:prefab)
#|
(mod-name code)
(mod-srcname code)
(mod-self-modidx code)
(mod-prefix code)

(mod-provides code) ; each phase maps to two lists: exported variables, exported syntax
(mod-requires code) ; each phase maps to a list of imported module paths
(mod-body code) ; run-time (phase 0) code -> list of form?
(mod-syntax-bodies code)
(mod-pre-submodules code)
|#

(define prefs (mod-prefix code))
(define toplevels (prefix-toplevels prefs))

(define topsyntaxes (prefix-stxs prefs))

(define top-provides (cadr (assv 0 (mod-provides code))))
(define syntax-phase-provides (caddr (assv 0 (mod-provides code))))

(define all-provides '());(append top-provides syntax-phase-provides))

(define (handle-provides provs out)
  (cond
    [(null? provs) out]
    [else
     (let*
         ([current-module-path (string-append relative-current-dir module-name ".rkt")]
          [pr (car provs)]
          [out-name (symbol->string (provided-name pr))]
          [src (provided-src pr)]
          [src-path (if (not src) ;; then it's from our current module
                        current-module-path
                        (let ([path (resolved-module-path-name
                                     (module-path-index-resolve src))])
                          (if (symbol? path)
                              (symbol->string path)
                              (path->string path))))]
          [orig-name (symbol->string (provided-src-name pr))]
          [nom-src (provided-nom-src pr)] ;; <- interesting that this is a list
          [nom-src-path (if (not src)
                            current-module-path
                            (let ([path (resolved-module-path-name
                                         (module-path-index-resolve
                                          (begin (when (> (length nom-src) 1) (error 'handle-provides "we got more than one nom-srcs"))
                                                 (car nom-src))))])
                              (if (symbol? path)
                                  (symbol->string path)
                                  (path->string path))))]
          [provided-ast-node (if (string=? out-name orig-name)
                                 ;; no rename-out
                                 (hash* 'source-name out-name
                                        'source-module src-path)
                                 ;; rename-out
                                 (list (hash* 'toplevel "rename")
                                       (hash* 'source-name orig-name
                                              'source-module nom-src-path)
                                       (hash* 'toplevel out-name)))])
       (handle-provides (cdr provs)
                        (cons provided-ast-node out)))]))

(define topProvides '()); (handle-provides all-provides '()))



(define (self-mod? mpi)
    (let-values ([(mod-path base-path) (module-path-index-split mpi)])
      (and (not mod-path) (not base-path))))

(define relative-current-dir (path->string (current-directory)))
(define (module-path-index->path-string mod-idx)
  (if (self-mod? mod-idx)
      (string-append relative-current-dir module-name ".rkt")
      (let-values ([(module-path base-path) (module-path-index-split mod-idx)])
        (if (list? module-path) ;; it may be resolved
            (with-handlers ([exn:fail? (λ (e) (map (λ (s) (if (symbol? s) (symbol->string s) s)) (cdr module-path)))])
              (path->string (resolved-module-path-name (module-path-index-resolve mod-idx))))
            (if (symbol? module-path) ;; then it is resolved
                (let ([path (resolved-module-path-name (module-path-index-resolve mod-idx))])
                  (if (symbol? path)
                      (symbol->string path)
                      (path->string path)))
                (if (not (string? module-path))
                    (error 'module-path-index->path-string "module-path is not a list, symbol or string : ~a, in ~a" module-path mod-idx)
                    ;; resolving manually using the base-path
                    (if (self-mod? base-path)
                        (string-append relative-current-dir module-path)
                        (let ([base-path-str (if (resolved-module-path? base-path)
                                                 (path->string (resolved-module-path-name base-path))
                                                 (module-path-index->path-string base-path))])
                          (begin ;; sanity-check : should end with .rkt
                            (when (not (string-suffix? base-path-str ".rkt"))
                              (error 'module-path-index->path-string "something's wrong with the resolved base path : ~a" base-path-str))
                            (let* ([spl (string-split base-path-str "/")]
                                   [real-base (string-append "/" (string-join (take spl (sub1 (length spl))) "/") "/")])
                              (string-append real-base module-path)))))))))))
  ;; (display " ------------------------------------------- ")(newline)
  ;; (display (string-append subDirsStr "compiled/" moduleName "_rkt.dep"))
  ;; (display " ------------------------------------------- ")(newline)


;; language          (language . ("/home/caner/programs/racket/collects/racket/main.rkt"))
;; langDep -> '(collects #"racket" #"main.rkt")

;; (define lang (string-append collectsDir "/racket" "/main.rkt"))
(define topRequires (mod-requires code)) ;; assoc list ((phase mods) ..)
(define phase0 (assv 0 topRequires))
(define phase0-reqs (cdr phase0))
#;(define langModPath (resolved-module-path-name
                     (module-path-index-resolve (car phase0-reqs))))
#;(define lang (if (list? langModPath)
                 (error 'langConfig "don't know how to handle a submodule here")
                 (if (symbol? langModPath)
                     (symbol->string langModPath)
                     (path->string langModPath))))

(define lang (module-path-index->path-string (car phase0-reqs)))

(define lang-pycket?
  (let ([lang* (if (string? lang) lang
                   (if (and (list? lang) (null? (cdr lang))) ;; (quoted mpi) like '("#%kernel")
                       (car lang)
                       (error 'lang-pycket? "unusual lang form : ~a" lang)))])
    (or (string=? lang* "#%kernel") (string-contains? lang* "pycket-lang"))))

(define body1
  (if lang-pycket?
      'dont-care
      (let* ([preSubmods (mod-pre-submodules code)]
             [runtimePrefix (mod-prefix (car preSubmods))]
             [runtimeMod (car (filter module-variable? (prefix-toplevels runtimePrefix)))]
             ;; assert (module-variable? runtimeMod) and (eqv? module-variable-sym 'configure)
             [resolvedModPath (resolved-module-path-name
                               (module-path-index-resolve (module-variable-modidx runtimeMod)))]
             [runtimeConfig (if (or (list? resolvedModPath)
                                    (symbol? resolvedModPath))
                                (error 'runtimeConfigModule "don't know how to handle a submodule here")
                                (path->string resolvedModPath))])
        (hash* 'language (list "#%kernel")
               'module-name (symbol->string (mod-srcname (car preSubmods)))
               'body-forms (list
                            (hash* 'require (list (list runtimeConfig)))
                            (hash* 'operator (hash* 'source-module (list runtimeConfig)
                                                    'source-name (symbol->string
                                                                  (module-variable-sym runtimeMod)))
                                   'operands (list (hash 'quote #f))))))))

(define reqs (cdr phase0-reqs))

(define top-level-req-forms
    (map (λ (req-mod)
           (let ([mod-path (module-path-index->path-string req-mod)])
             (hash* 'require (list (if (list? mod-path) mod-path (list mod-path)))))) reqs))

 ;; code-body : listof def-values
  (define (prepare-toplevels code-body)
    (let* ([defvals (filter def-values? code-body)]
           [new-toplevels (collect-toplevels defvals)]) ;; <-- ((pos-num top-sym) ...)
      (if (null? new-toplevels)
          toplevels ;; don't bother
          (let*
              ([toplen (length toplevels)]
               ;; aligning new toplevels like ((30 x) (32 a) (35 b)) ==> ((30 x) (31 dummy) (32 a) (33 dummy) (34 dummy) (35 b))
               ;; we know that there's no reference in the code to the missing toplevels, but the position matters (for list-ref)
               ;; TODO : revisit : use a hashmap for toplevels
               [aligned-new-toplevels (foldr (λ (x rest)
                                               (if (or (null? rest)
                                                       (= 1 (- (caar rest) (car x))))
                                                   (cons x rest)
                                                   (let ([diff (- (caar rest) (car x) 1)])
                                                     (cons x (append (build-list diff (λ (n) (list (+ n x 1) 'dummytop))) rest))))) null new-toplevels)]

               [padded-new-toplevels (let* ([diff (- (caar aligned-new-toplevels) toplen)]
                                            [pad (build-list diff (λ (n) (list (+ n toplen) 'dummytop)))])
                                       (append pad aligned-new-toplevels))])
                                       
            ;; at this point, we know everything's in place,
            ;; so we can safely append prepared new-toplevels to the current global toplevels
            (let ([top-syms (map cadr padded-new-toplevels)])
              (append toplevels top-syms))))))
  
  ;; collect-toplevels : (listof def-values) -> (listof pos-num top-var-sym)
  (define (collect-toplevels code-body)
    (sort
     (filter
      (compose not null?)
      (map (λ (defval)
             (let* ([def-ids (def-values-ids defval)]
                    [def-rhs (def-values-rhs defval)]
                    [poss (map toplevel-pos def-ids)]
                    [toplen (length toplevels)]) ; <- this is the real prefix-toplevels from comp-top
               (if (= (length poss) 1)
                   (if (< (car poss) toplen)
                       '()
                       (let ([sym (let ([name (cond [(lam? def-rhs) (lam-name def-rhs)]
                                                    [(inline-variant? def-rhs) (lam-name (inline-variant-direct def-rhs))]
                                                    [else (error 'collect-toplevels "couldn't get the name from ~a" defval)])])
                                    (if (symbol? name) name (gensym (vector-ref name 0))))])
                         (list (car poss) (gensym sym))))
                   ;; we have multiple toplevels at the defval
                   (if (ormap (λ (pos) (>= pos toplen)) poss)
                       (error 'prepare-toplevels "one of the ids have >toplevel pos : ~a in defval : " poss defval)
                       ;; then all the top-posses are < toplen, thus we ignore
                       '()))))
           code-body))
     (λ (l1 l2) (< (car l1) (car l2)))))

(define (compile-json2 config language topmod body1 top-require-forms body-forms pycket?)
  (hash* 'language (list language)
         'module-name topmod
         'config config
         'body-forms (if pycket?
                         (append top-require-forms body-forms)
                         (cons body1 (append top-require-forms body-forms)))))
;; body-forms is a (listof hash hash)

;(define config global-config)
;(define language lang)



; (mod-body code)



#;(define final-json-hash (if GENERATE-AST
                            (compile-json2 global-config
                                           (if lang-pycket? "#%kernel" lang)
                                           modName
                                           body1
                                           top-level-req-forms ;(append toplevelRequireForms topProvides)
                                           (to-ast-wrapper (mod-body code) (prepare-toplevels (mod-body code)) topsyntaxes DEBUG modName relative-current-dir)
                                           lang-pycket?)
                            #f))
            
;(jsexpr? final-json-hash)

#|
(define out (open-output-file (string-append "inspector_" modName ".rkt.json")
                              #:exists 'replace))
(begin
  (display (string-append "WRITTEN: inspector_" modName ".rkt.json\n\n"))
  (write-json final-json-hash out)
  (newline out)
  (flush-output out))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define benchs '(divrec sum sumfp triangl fib fibfp tak ack mazefun nqueens gcbench nucleic))

#;(when BENCH-ASTS
  (let* ([byte-pre "asts/bytes-no-inline/"]
         [pycket-pre "asts/regulars/"]
         )
    (begin 
      (for/list ([b benchs])
        (let* ([b-json (format "~a~a-byte.json" byte-pre b)]
               [p-json (format "~a~a-pycket.json" pycket-pre b)]
               [b-ast (format "~a~a-byte.ast" byte-pre b)]
               [p-ast (format "~a~a-pycket.ast" pycket-pre b)])
          (sexp->file (json-file->sexp b-json) b-ast)
          #;(sexp->file (json-file->sexp p-json) p-ast)))
      (displayln "Asts are pretty written....\n")
      (displayln "AST Node counts:")
      (displayln "-- For Bytecodes :")
      (for/list ([b benchs])
        (displayln (format "~a : ~a" b (count-nodes (json-file->sexp (format "~a~a-byte.json" byte-pre b))))))
      #;(displayln "-- For Regular Pycket :")
      #;(for/list ([b benchs])
        (displayln (format "~a : ~a" b (count-nodes (json-file->sexp (format "~a~a-pycket.json" pycket-pre b))))))
      )))

#lang racket/base

(provide generate pick-a-value)

;; Random S-Exp generator

(define generators (make-hash))

(hash-set! generators 'integer
           (lambda () (random 1000)))
(hash-set! generators 'flonum
           (lambda () (* (random 100) 2.37)))
(hash-set! generators 'rational
           (lambda () (/ 11 (+ 12 (random 100)))))
(hash-set! generators 'complex
           (lambda () (make-rectangular (random 100) (random 100))))
(hash-set! generators 'symbol
           (lambda () (gensym)))
(hash-set! generators 'string
           (lambda () (symbol->string (gensym))))
(hash-set! generators 'char
           (lambda () (integer->char (random 1000))))
(hash-set! generators 'boolean
           (lambda () (if (= 0 (random 2)) #t #f)))
(hash-set! generators 'path
           (lambda () (apply build-path
                             (build-list
                              (+ 1 (random 10))
                              (lambda (x)
                                (symbol->string (gensym)))))))
(hash-set! generators 'regexp
           (lambda () (regexp (symbol->string (gensym)))))

(hash-set! generators 'box
           (lambda () (box (pick-a-value))))


#|
(hash-set! generators 'list
           (lambda () (build-list (random 100) (lambda (x) (pick-a-value)))))
(hash-set! generators 'vector
           (lambda () (list->vector ((hash-ref generators 'list)))))
(hash-set! generators 'pair
           (lambda () ((hash-ref generators 'list))))
(hash-set! generators 'hash
           (lambda () (let ([ls ((hash-ref generators 'list))])
                        (make-hash (map cons ls ls)))))
|#

(define (pick-a-value [override-key #f])
  (let ([pick-a-key
         (if override-key override-key
             (list-ref (hash-keys generators)
                       (random (hash-count generators))))])
    ((hash-ref generators pick-a-key))))
     
; let's not worry about nesting for now
; just generate a flat list of values
(define (generate size [override-key #f])
  (build-list size (lambda (x) (pick-a-value override-key))))
    

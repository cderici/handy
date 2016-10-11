#lang racket

(require 2htdp/batch-io)

(define trace-confs "../confidence-intervals/trace-confs")
(define no-trace-confs "../confidence-intervals/no-trace-confs")
(define extra-param-confs "../confidence-intervals/extra-param-confs")

(define modes '(regular byte bni))

(define setting extra-param-confs)

(define benchmarks '(sum sumfp sumtail sumrec sumrecfp tak takl triangl
                         sumloop divrec diviter fib fibfp fibc ctak cpstak ack nqueens
                         nucleic earley paraffins primes gcbench string pi deriv mbrot
                         fft array1 pnpoly))

(let* ([f (read-file setting)]
       [lines** (string-split f "\n")]
       [lines* (map (lambda (l) (string-split l " ")) lines**)]
       [lines (map (lambda (l) (list (list-ref l 0)
                                (string->number (list-ref l 2))
                                (string->number (list-ref l 4)))) lines*)]
       [regularS* (filter (lambda (l) (string-contains? (car l) "regular")) lines)]
       [byteS* (filter (lambda (l) (string-contains? (car l) "byte")) lines)]
       [bniS* (filter (lambda (l) (string-contains? (car l) "bni")) lines)]

       [regularS (for/list ([bench* benchmarks])
                   (let* ([bench (symbol->string bench*)]) ;; v- this is ridiculus 
                     (car (filter (lambda (l) (string=? (car l) (format "~a-extra-param-regular" bench))) regularS*))))]

       [byteS (for/list ([bench* benchmarks])
                   (let* ([bench (symbol->string bench*)])  
                     (car (filter (lambda (l) (string=? (car l) (format "~a-extra-param-byte" bench))) byteS*))))]

       [bniS (for/list ([bench* benchmarks])
                   (let* ([bench (symbol->string bench*)]) 
                     (car (filter (lambda (l) (string=? (car l) (format "~a-extra-param-bni" bench))) bniS*))))])

  (begin
    (for ([r regularS])
      (displayln (format "~a ~a" (cadr r) (caddr r))))
    (displayln (newline))
    (for ([by byteS])
      (displayln (format "~a ~a" (cadr by) (caddr by))))
    (displayln (newline))
    (for ([b bniS])
      (displayln (format "~a ~a" (cadr b) (caddr b))))
    (displayln (newline))
    'done))
                          
  

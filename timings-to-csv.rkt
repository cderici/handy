#lang racket

(require "analyze-rebench-output.rkt")

(provide benchmarks MODES results->list)

#;(define REL_DIR "../timings-no-trace/")

;; order is the one on the spreadsheet, needs to be kept in sync
(define benchmarks '(sum sumfp sumtail sumrec sumrecfp tak takl triangl
                         sumloop divrec diviter fib fibfp fibc ctak cpstak ack nqueens
                         nucleic earley paraffins primes gcbench string pi deriv mbrot
                         fft array1 pnpoly))


(define MODES '(regular byte bni)) ;; order is important here as well

;; rst files should be in the same directory
(define (results->list REL_DIR)
  (for/list ([bench benchmarks])
    (for/list ([mode MODES])
      (let ([rst-file (format "~a~a-~a.rst" REL_DIR bench mode)])
        (rst-file->time-list rst-file)))))


;(define l1 (map cons benchmarks (results->list "../trace-log-10-times/")))
;(define l2 (map cons benchmarks (results->list "../no-trace-log-10-times/")))
;(define l3 (map cons benchmarks (results->list "../extra-params-no-trace-10-times/")))



 ;(define ls (results->list)) ;; (listof (listof regular byte bni) ...)
 
(define (append-and-serialize ls)
  (apply (curry map (lambda l l)) (foldr append null ls)))

(define (write-to-csv csv-file-name csv-ready-list)
  (call-with-output-file csv-file-name
    (lambda (op)
      (for ([row csv-ready-list])
        (displayln (string-join (map number->string row) ";") op)))
    #:exists 'replace))

;(define ls (results->list))

#;(write-to-csv "benchmark-results.csv" (append-and-serialize (results->list)))

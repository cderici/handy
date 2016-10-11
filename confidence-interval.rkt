#lang racket

(require "timings-to-csv.rkt"
         math/statistics)

(define (vector-vector-sort v c)
  (list->vector (sort (vector->list v)
                      (lambda (v1 v2) (c (vector-ref v1 0)
                                    (vector-ref v2 0))))))

;; bootstrap conf interval
;;  v---> allows to avoid assuming anything about the distribution

;; assumes (vector-length data-vector) : 1000
(define (ci-95 data-vector)
  ;; init:
  ;; -- Compute mean X^ and standard deviation S of the original data
  (let* ([X^ (mean data-vector)]
         [S (stddev data-vector)]
         [sample-size 1000]
  
         ;; bootstrap -> repeat R times (R:1000)
         [bootstrap-stats*
          (for/vector ([i (range 1000)])
      
           ;; resample with replacement n = 1000
           (let* ([sample (for/vector ([i (range sample-size)])
                            (vector-ref data-vector (random data-vector)))]
                  ;; for each compute sample mean x^, sample stddev s^,
                  ;; and sample t-b (/ (x^ - X^) (/ s^ (sqrt n)))
                  [x^ (mean sample)]
                  [s^ (stddev sample)]
                  [tb (/ (- x^ X^) (/ s^ (sqrt sample-size)))])
             (vector tb x^ s^)))]
         ;; produce
         ;; sorted (ascending) (vector (t-pivot sample-mean sample-var) of R length) containing
         ;;              -- sample mean, sample variance, sample t-pivot values
         [bootstrap-stats (vector-vector-sort bootstrap-stats* <)])
    
    ;; pick t25th and t975th elements
    (let ([t25 (vector-ref (vector-ref bootstrap-stats 25) 0)]
          [t975 (vector-ref (vector-ref bootstrap-stats 975) 0)])

      ;; Compute [(- X^ (* t25 (/ S (sqrt n)))), (- X^ (* t975 (/ S (sqrt n))))]
      ;; produce (values lower upper) confidence limits
      (values (- X^ (* t975 (/ S (sqrt sample-size)))) (- X^ (* t25 (/ S (sqrt sample-size))))))))

(module+ main
  (define bench #f)
  (define settings (list "trace-log-10-times/"
                         "no-trace-log-10-times/"
                         "extra-params-no-trace-log-10-times/"))
  (define setting# #f)
  (define setting-print '(trace no-trace extra-param))
  
  (command-line
   #:args (bench-arg* setting*)
   (let ([setting (string->number setting*)]
         [bench-arg (string->symbol bench-arg*)])
     (begin
       (when (not (memq bench-arg benchmarks))
         (error 'confidence-interval "invalid benchmark argument : ~a " bench-arg))
       (when (or (not (number? setting)) (not (memq setting '(0 1 2))))
         (error 'confidence-interval "invalid setting argument (must be 0 or 1 or 2)"))
       (set! bench bench-arg)
       (set! setting# setting))))

  ;(define bench 'sum)
  (when (or (not bench) (not setting#))
    (error 'confidence-interval "check the arguments"))

  (define setting-out (list-ref setting-print setting#))

  (define trace-log (map cons benchmarks (results->list (list-ref settings setting#))))

  (display (map (lambda (t) (list (car t) (length (cadr t)) (length (caddr t)) (length (cadddr t)))) trace-log))

  ;; '((sum (<data:regular>1000) (<data:byte>1000) (<data:bni>1000))
  ;;   (sumfp (<data:regular>1000) (<data:byte>1000) (<data:bni>1000))
  ;;   ...)

  (define select-data-set (car (filter (lambda (l) (equal? (car l) bench)) trace-log)))

  (define data-regular (list->vector (list-ref select-data-set 1)))
  (define data-byte (list->vector (list-ref select-data-set 2)))
  (define data-bni (list->vector (list-ref select-data-set 3)))

  (let-values ([(reg-low reg-high) (ci-95 data-regular)]
               [(byte-low byte-high) (ci-95 data-byte)]
               [(bni-low bni-high) (ci-95 data-bni)])
    (displayln (format "~a-~a-regular : ~a - ~a" bench setting-out reg-low reg-high))
    (displayln (format "~a-~a-byte : ~a - ~a" bench setting-out byte-low byte-high))
    (displayln (format "~a-~a-bni : ~a - ~a" bench setting-out bni-low bni-high)))
  
  )

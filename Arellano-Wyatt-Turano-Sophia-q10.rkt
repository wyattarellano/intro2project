#lang htdp/asl
;;An lon is either:
;; 1. '()
;; 2. (cons num lon)

;;sample instances of lon
(define LON1 '(1 10 3 100 2 55 6 7777 10))
(define LON2 '(80 10 3 100 2 55 6 5 10))
(define LON3 '(1 10 3 100 2 55 6 440))
(define EMP-LON '())

;;lon -> number
;;Purpose: To find the max of the given lon
(define (max-lon! a-lon)
  (if (empty? a-lon)
      (error "No max for an empty list")
      (local[;;number 
             ;;Purpose: Value of the max of the processed list
             (define accum (void))
             ;;lon
             ;;Purpose: Unprocessed part of the given lon
             (define ul (void))
             ;;-> number
             ;;Purpose: To find the max of a lon
             ;;Effect: Accum is changed to the first of the unprocessed list if the first of the unprocessed list
             ;;        is greater than accum
             ;;Accumulator Invariant: accum = current max of the processed list
             (define (max-state!)
               ;;accum = current max of (a-lon - ul)
               (cond[(empty? ul) ;;accum = current max of (a-lon - ul) AND ul = empty -> accum = max
                     accum]
                    [(< accum (first ul)) (begin
                                            ;;accum = current max of (a-lon - ul) AND ul = not empty
                                            (set! accum (first ul))
                                            ;;accum = current max of (a-lon - (rest ul)) AND ul = not empty 
                                            (set! ul (rest ul))
                                            ;;accum = current max of (a-lon - ul)
                                            (max-state!))]
                    [else (begin
                            ;;accum = current max of (a-lon - (rest ul)) AND ul = not empty
                            (set! ul (rest ul))
                            ;;accum = current max of (a-lon - ul)
                            (max-state!))]))
             ;;Termination argument:
             ;;The unprocessed list has its length reduced by one every recursive call
             ;;due to setting ul to the rest of ul. Eventually ul becomes
             ;;empty and this recursion terminates.
             ]
        
        (begin
          (set! accum -inf.0)
          (set! ul a-lon)
          (max-state!)))))


;;Sample expressions
(define MAX-VAL1 (local[;;number 
             ;;Purpose: Value of the max of the processed list
             (define accum (void))
             ;;lon
             ;;Purpose: Unprocessed part of the given lon
             (define ul (void))
             ;;-> number
             ;;Purpose: To find the max of a lon
             ;;Effect: Accum is changed to the first of the unprocessed list if the first of the unprocessed list
             ;;        is greater than accum
             ;;Accumulator Invariant: accum = current max of the processed list
             (define (max-state!)
               ;;accum = current max of (a-lon - ul)
               (cond[(empty? ul) ;;accum = current max of (a-lon - ul) AND ul = empty -> accum = max
                     accum]
                    [(< accum (first ul)) (begin
                                            ;;accum = current max of (a-lon - ul) AND ul = not empty
                                            (set! accum (first ul))
                                            ;;accum = current max of (a-lon - (rest ul)) AND ul = not empty 
                                            (set! ul (rest ul))
                                            ;;accum = current max of (a-lon - ul)
                                            (max-state!))]
                    [else (begin
                            ;;accum = current max of (a-lon - (rest ul)) AND ul = not empty
                            (set! ul (rest ul))
                            ;;accum = current max of (a-lon - ul)
                            (max-state!))]))
             ;;Termination argument:
             ;;The unprocessed list has its length reduced by one every recursive call
             ;;due to setting ul to the rest of ul. Eventually ul becomes
             ;;empty and this recursion terminates.
             ]
        
        (begin
          (set! accum -inf.0)
          (set! ul LON1)
          (max-state!))))

(define MAX-VAL2 (local[;;number 
             ;;Purpose: Value of the max of the processed list
             (define accum (void))
             ;;lon
             ;;Purpose: Unprocessed part of the given lon
             (define ul (void))
             ;;-> number
             ;;Purpose: To find the max of a lon
             ;;Effect: Accum is changed to the first of the unprocessed list if the first of the unprocessed list
             ;;        is greater than accum
             ;;Accumulator Invariant: accum = current max of the processed list
             (define (max-state!)
               ;;accum = current max of (a-lon - ul)
               (cond[(empty? ul) ;;accum = current max of (a-lon - ul) AND ul = empty -> accum = max
                     accum]
                    [(< accum (first ul)) (begin
                                            ;;accum = current max of (a-lon - ul) AND ul = not empty
                                            (set! accum (first ul))
                                            ;;accum = current max of (a-lon - (rest ul)) AND ul = not empty 
                                            (set! ul (rest ul))
                                            ;;accum = current max of (a-lon - ul)
                                            (max-state!))]
                    [else (begin
                            ;;accum = current max of (a-lon - (rest ul)) AND ul = not empty
                            (set! ul (rest ul))
                            ;;accum = current max of (a-lon - ul)
                            (max-state!))]))
             ;;Termination argument:
             ;;The unprocessed list has its length reduced by one every recursive call
             ;;due to setting ul to the rest of ul. Eventually ul becomes
             ;;empty and this recursion terminates.
             ]
        
        (begin
          (set! accum -inf.0)
          (set! ul LON2)
          (max-state!))))

(define MAX-VAL3 (local[;;number 
             ;;Purpose: Value of the max of the processed list
             (define accum (void))
             ;;lon
             ;;Purpose: Unprocessed part of the given lon
             (define ul (void))
             ;;-> number
             ;;Purpose: To find the max of a lon
             ;;Effect: Accum is changed to the first of the unprocessed list if the first of the unprocessed list
             ;;        is greater than accum
             ;;Accumulator Invariant: accum = current max of the processed list
             (define (max-state!)
               ;;accum = current max of (a-lon - ul)
               (cond[(empty? ul) ;;accum = current max of (a-lon - ul) AND ul = empty -> accum = max
                     accum]
                    [(< accum (first ul)) (begin
                                            ;;accum = current max of (a-lon - ul) AND ul = not empty
                                            (set! accum (first ul))
                                            ;;accum = current max of (a-lon - (rest ul)) AND ul = not empty 
                                            (set! ul (rest ul))
                                            ;;accum = current max of (a-lon - ul)
                                            (max-state!))]
                    [else (begin
                            ;;accum = current max of (a-lon - (rest ul)) AND ul = not empty
                            (set! ul (rest ul))
                            ;;accum = current max of (a-lon - ul)
                            (max-state!))]))
             ;;Termination argument:
             ;;The unprocessed list has its length reduced by one every recursive call
             ;;due to setting ul to the rest of ul. Eventually ul becomes
             ;;empty and this recursion terminates.
             ]
        
        (begin
          (set! accum -inf.0)
          (set! ul LON3)
          (max-state!))))

;;Tests using sample computations
(check-expect (max-lon! LON1) MAX-VAL1)
(check-expect (max-lon! LON2) MAX-VAL2)
(check-expect (max-lon! LON3) MAX-VAL3)
(check-error (max-lon! EMP-LON) "No max for an empty list")

;;Tests using sample values
(check-expect (max-lon! '(1 5 10 2 18)) 18)
(check-expect (max-lon! '(20 5 10 2 18)) 20)
(check-expect (max-lon! '(1 5 345 2 18)) 345)
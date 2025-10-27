#lang htdp/asl
(require while)

;; A (vectorof integers) (voi) is either:
(define V0 (vector))
(define V1 (vector 10 3 7 17 11))
(define V2 (vector 31 46 60 22 74 22 27 60 20 44 23 85 86 67 12 75 80 77 62 37))
(define V3 (build-vector 10 (λ (i) i)))
(define V4 (build-vector 10 (λ (i) (sub1 (- 10 i)))))
(define V5 (vector -1 8 19 -1008 32 -19))


;;a-voi -> integer
;;Purpose: To find the number of digits in the longest integer in a (vectorof integer) 
(define (num-of-digits a-voi)
  (if (= (vector-length a-voi) 0)
      0
      (local [;;natnum
              ;;Purpose: Low value of interval being search
              (define low (void))

              ;;natnum
              ;;Purpose: High value of interval being searched
              (define high (void))

              ;;natnum
              ;;Purpose: Length of the longest integer in the processed interval from [V[0]... (sub1 low)]
              (define accum (void))

              ;;->number
              ;;Purpose: To find the number of digits in the longest integer
              ;;Effect: accum is changed to (string-length(number->string(abs (vector-ref a-voi low))))
              ;;        when accum < (string-length(number->string(abs (vector-ref a-voi low)))).
              ;;        Otherwise, accum stays the same. low is always incremented by one during each recursive call.
              ;;INV: high = (sub1 (vector-length a-voi)) AND
              ;;     accum = length of the longest integer in [V[0]... (sub1 low)] AND
              ;;     (vector-length a-voi) = [V[0] ... V[(sub1 low)]] + [V[low] ... V[high]]
              ;;Termination Argument: If low is less than high, the function returns accum. If accum is
              ;;                      less than the length of the length of the number at V[low], then
              ;;                      low is incremented by 1 and the function is recursively called. 
              ;;                      Otherwise, low is incremented by 1 and the function is recursively
              ;;                      called. With each recursive call, low is incremented by 1, and will
              ;;                      eventually become greater than high, thus terminating the loop. 
              (define (digit-helper!)
                
                (cond [(> low high)
                       accum]
                      [(< accum (string-length(number->string(abs (vector-ref a-voi low)))))
                       ;; INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[sub1 low]]
                       ;;      AND low < high
                       (begin
                         ;;INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[sub1 low]]
                         (set! accum (string-length(number->string(abs (vector-ref a-voi low)))))
                         ;; INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[low]]
                         ;;      AND low < high
                         (set! low (add1 low))
                         ;; INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[sub1 low]]
                         (digit-helper!)
                         ;; INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[sub1 low]]
                         )]
                      [else
                       ;; INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[low]]
                       ;;      AND low < high
                       (begin
                         ;; INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[low]]
                         ;;      AND low < high
                         (set! low (add1 low))
                         ;; INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[sub1 low]]
                         (digit-helper!)
                         ;; INV: high = (sub1 (vector-length a-voi)) AND accum is the length of the longest integer found in [V[0]...V[low]]
                         )]))]
        (begin
          (set! low 0)
          (set! high (sub1 (vector-length a-voi)))
          (set! accum 0)
          (digit-helper!)))))



;;--> bucket
;; Purpose: To construct a bucket
(define (make-bucket)
  (local [;;(vectorof integers)
          ;;Purpose: Store the values in a bucket
          (define bucket (void))

          ;;number
          ;;Purpose: Number of elements in a bucket
          (define bucket-size (void))

          ;;voi --> (void)
          ;;Purpose: To initialize the bucket
          (define (init! vector) (begin
                                   (set! bucket (make-vector (vector-length vector) (void)))
                                   (set! bucket-size 0)))

          ;; -> number
          ;;Purpose: To return the number of elements in a bucket
          (define (size!) bucket-size)
            
          
          ;; --> (void)
          ;;Purpose: Add a given integer to the bucket
          ;;Effect: Bucket size is incremented by one AND the given number is added to the end of the vector
          (define (add! number)
            (begin
              (vector-set! bucket bucket-size number)
              (set! bucket-size (add1 bucket-size))))

          ;; voi number -> (void)
          ;; Purpose: To dump a bucket's elements into a given vector at the given index, and mutates
          ;;          the bucket to become empty
          (define (dump! vector index-of-vector)
            (local [;;number
                    ;;Purpose: The index of the given vector to dump the bucket's elements into
                    (define indx (void))

                    ;;number
                    ;;Purpose: The current index in the bucket that is being dumped
                    (define indx-of-bucket (void))

                    ;;voi -> (void)
                    ;;Purpose: To dump a bucket's elements into a given vector at the given index, and mutates
                    ;;          the bucket to become empty
                    ;;Effect: vector[indx] is changed to (vector-ref bucket indx-of-bucket) and Bucket[indx-of-bucket] is changed to void
                    ;;        and indx-of-bucket and indx are incremented by 1.
                    (define (dump-helper! vector)
                      (if (> indx-of-bucket (sub1 bucket-size))
                          (set! bucket-size 0)
                          
                       
                          (begin
                            ;; (<= indx-of-bucket (sub1 bucket-size)) AND (not (equal? (vector-ref bucket indx-of-bucket) (void)))
                                
                            (vector-set! vector indx (vector-ref bucket indx-of-bucket))

                            ;; (< indx-of-bucket (sub1 bucket-size)) AND (not (equal? (vector-ref bucket indx-of-bucket) (void)))
                            ;; AND V[indx] = (vector-ref bucket indx-of-bucket)
                            (vector-set! bucket indx-of-bucket (void))

                            ;; (<= indx-of-bucket (sub1 bucket-size)) AND (not (equal? (vector-ref bucket indx-of-bucket) (void)))
                            ;; AND V[indx] = (vector-ref bucket indx-of-bucket) AND bucket[indx-of-bucket] = (void)                             
                            (set! indx-of-bucket (add1 indx-of-bucket))

                            ;; (<= indx-of-bucket (sub1 bucket-size))
                            ;; AND V[indx] = (vector-ref bucket (sub1 indx-of-bucket))
                            (set! indx (add1 indx))

                            ;; (<= indx-of-bucket (sub1 bucket-size))
                            ;; AND V[(sub1 indx)] = (vector-ref bucket (sub1 indx-of-bucket))
                            (dump-helper! vector))))]
              (begin
                (set! indx index-of-vector)
                (set! indx-of-bucket 0)
                (dump-helper! vector))))

         

          ;; -> voi
          ;;Purpose: To return a vector containing the elements of a bucket
          (define (elems)
            bucket)

          ; Message --> bucket throws error
          ; Purpose: To manage a-voi
          (define (bucket-object a-message)
            (cond [(eq? a-message 'size) size!]
                  [(eq? a-message 'add!) add!]
                  [(eq? a-message 'initialize!) init!]
                  [(eq? a-message 'elems) elems]
                  [(eq? a-message 'dump!) dump!]
                  [else (error 'bucket-object
                               (format "Unknown message received: ~s"
                                       a-message))]))]
    bucket-object))

;; Sample bucket
(define BUCKET1 (make-bucket))
(define BUCKET2 (make-bucket))
(define BUCKET3 (make-bucket))


;; bucket vector --> (void)
;; Purpose: To initialize a bucket
(define (init-bucket! bucket vector) ((bucket 'initialize!) vector))

;; number bucket --> (void)
;; Purpose: To add an integer to a bucket
(define (add! integer bucket)
  ((bucket 'add!) integer))

;;a-voi number bucket -> (void)
;;Purpose: To dump a buckets elements into a vector at the given index
(define (dump! vector index bucket)
  ((bucket 'dump!) vector index))

;;bucket -> number
;;Purpose: Return the size of a bucket
(define (size bucket)
  ((bucket 'size)))

;;bucket -> voi
;;Purpose: To return a vector containing the bucket elements
(define (elems bucket)
  ((bucket 'elems)))






;                                                                 
;                                                                 
;                        ;  ;                                     
;   ;;;;;;              ;;                  ;;; ;                 
;    ;    ;              ;                 ;   ;;              ;  
;    ;    ;              ;                 ;    ;              ;  
;    ;    ;    ;;;    ;; ;  ; ;;; ;;;      ;        ;;;   ; ;;;;;;
;    ;;;;;    ;   ;  ;  ;; ;;  ;   ;        ;;     ;   ; ;;;   ;  
;    ;  ;         ;  ;   ;  ;   ; ;           ;;   ;   ;  ;    ;  
;    ;   ;     ;;;;  ;   ;  ;    ;    ;;;       ;  ;   ;  ;    ;  
;    ;   ;    ;   ;  ;   ;  ;   ; ;        ;    ;  ;   ;  ;    ;  
;    ;    ;   ;   ;  ;  ;;; ;  ;   ;       ;;   ;  ;   ;  ;    ; ;
;   ;;;   ;;;  ;;;;;  ;; ; ;;;;;; ;;;      ; ;;;    ;;;  ;;;   ;; 
;                                                                 
;                                                                 
;                                                                 
;                                                                 

;;a-voi -> (void)
;;Purpose: Sort a given vector in non-decreasing order
;;Termination Argument:
;;If the vector length is less than 2, then there is no recursion. Otherwise, during each recursive call of the function the expo-val
;;is incremented by 1. This continues until the expo-val is equal to MAX-DIGITS, the amount of times the fuction is recursively called.
;;When expo-val is equal to MAX-DIGITS, the function returns (void). Thus, terminating.
(define (radix-sort! a-voi)
  (if (< (vector-length a-voi) 2)
      (void)
      (local [
              ;; number
              ;; Purpose: Amount of digits in the longest integer in the vector
              (define MAX-DIGITS (num-of-digits a-voi))

              ;; number
              ;; Purpose: The low of the unprocessed interval 
              (define low (void))

              ;; number
              ;; Purpose: The high of the unprocessed interval
              (define high (void))

              ;; number
              ;; Purpose: The exponent that 10 is exponated to in order to find the number at a digit place
              (define expo-val (void))

              (define B--9 (make-bucket))
              (define B--8 (make-bucket))
              (define B--7 (make-bucket))
              (define B--6 (make-bucket))
              (define B--5 (make-bucket))
              (define B--4 (make-bucket))
              (define B--3 (make-bucket))
              (define B--2 (make-bucket))
              (define B--1 (make-bucket))
              
              (define B-0 (make-bucket))
              (define B-1 (make-bucket))
              (define B-2 (make-bucket))
              (define B-3 (make-bucket))
              (define B-4 (make-bucket))
              (define B-5 (make-bucket))
              (define B-6 (make-bucket))
              (define B-7 (make-bucket))
              (define B-8 (make-bucket))
              (define B-9 (make-bucket))

              ;;a-voi -> (void)
              ;;Purpose: Sort a given vector in non-decreasing order
              ;;Effect: If (not (= expo-val MAX-DIGITS)) AND low > high, then each bucket is dumped into the vector
              ;;        AND expo-val is incremented by one AND low is set to 0.
              ;;        If (not (= expo-val MAX-DIGITS)) AND low < high, then (vector-ref a-voi low) is added to the correct
              ;;        bucket AND low is incremented by 1.
              (define (radix-sort-helper! a-voi)
                ;; INV: (vector-length a-voi) > 1
                (if (= expo-val MAX-DIGITS)
                    (void)
                    (if (> low high)
                        (local [(define LENGTH-OF-V (vector-length a-voi))
                                (define size-9 (size B--9))
                                (define size-8 (+ size-9 (size B--8)))
                                (define size-7 (+ size-8 (size B--7)))
                                (define size-6 (+ size-7 (size B--6)))
                                (define size-5 (+ size-6 (size B--5)))
                                (define size-4 (+ size-5 (size B--4)))
                                (define size-3 (+ size-4 (size B--3)))
                                (define size-2 (+ size-3 (size B--2)))                             
                                (define size-1 (+ size-2 (size B--1)))
                                
                                (define size0 (+ size-1 (size B-0)))
                                (define size1 (+ size0 (size B-1)))
                                (define size2 (+ size1 (size B-2)))
                                (define size3 (+ size2 (size B-3)))
                                (define size4 (+ size3 (size B-4)))
                                (define size5 (+ size4 (size B-5)))
                                (define size6 (+ size5 (size B-6)))
                                (define size7 (+ size6 (size B-7)))
                                (define size8 (+ size7 (size B-8)))]
                          ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1
                          (begin
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1
                            (dump! a-voi 0 B--9)
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0
                            (if (>= size-9 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0
                                (dump! a-voi size-9 B--8))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            (if (>= size-8 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                (dump! a-voi size-8 B--7))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0

                            (if (>= size-7 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0
                                (dump! a-voi size-7 B--6))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 1 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0

                            (if (>= size-6 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0
                                (dump! a-voi size-6 B--5))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0

                            (if (>= size-5 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0
                                (dump! a-voi size-5 B--4))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0

                            (if (>= size-4 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                (dump! a-voi size-4 B--3))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0

                            (if (>= size-3 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0
                                (dump! a-voi size-3 B--2))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0

                            (if (>= size-2 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0
                                (dump! a-voi size-2 B--1))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0

                            (if (>= size-1 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0
                                (dump! a-voi size-1 B-0))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                          
                            (if (>= size0 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                (dump! a-voi size0 B-1))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0
                        
                            (if (>= size1 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0
                                (dump! a-voi size1 B-2))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0
                         
                            (if (>= size2 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0
                                (dump! a-voi size2 B-3))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0
                          
                            (if (>= size3 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0
                                (dump! a-voi size3 B-4))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                          
                            (if (>= size4 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                                (dump! a-voi size4 B-5))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0
                          
                            (if (>= size5 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0
                                (dump! a-voi size5 B-6))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0
                          
                            (if (>= size6 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0
                                (dump! a-voi size6 B-7))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0
                         
                            (if (>= size7 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0
                                (dump! a-voi size7 B-8))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                          
                            (if (>= size8 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                                (dump! a-voi size8 B-9))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0
                          

                            (set! expo-val (add1 expo-val))

                            ;; INV: expo-val <= MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0
                            (set! low 0)

                            ;; INV: expo-val <= MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0 AND (= low 0)
                           
                            (radix-sort-helper! a-voi)))
                        (local [(define BUCKET-NUM (remainder (quotient (vector-ref a-voi low) (expt 10 expo-val)) 10))]
                          (cond [(= BUCKET-NUM -9)
                                 ;; INV: (= BUCKET-NUM -9) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -9) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--9)
                                   ;; INV: (= BUCKET-NUM -9) AND (<= low high) AND (vector-ref a-voi low) ∈ B--9
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -9) AND (vector-ref a-voi (sub1 low)) ∈ B--9
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM -8)
                                 ;; INV: (= BUCKET-NUM -8) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -8) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--8)
                                   ;; INV: (= BUCKET-NUM -8) AND (<= low high) AND (vector-ref a-voi low) ∈ B--8
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -8) AND (vector-ref a-voi (sub1 low)) ∈ B--8
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM -7)
                                 ;; INV: (= BUCKET-NUM -7) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -7) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--7)
                                   ;; INV: (= BUCKET-NUM -7) AND (<= low high) AND (vector-ref a-voi low) ∈ B--7
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -7) AND (vector-ref a-voi (sub1 low)) ∈ B--7
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM -6)
                                 ;; INV: (= BUCKET-NUM -6) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -6) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--6)
                                   ;; INV: (= BUCKET-NUM -6) AND (<= low high) AND (vector-ref a-voi low) ∈ B--6
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -6) AND (vector-ref a-voi (sub1 low)) ∈ B--6
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM -5)
                                 ;; INV: (= BUCKET-NUM -5) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -5) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--5)
                                   ;; INV: (= BUCKET-NUM -5) AND (<= low high) AND (vector-ref a-voi low) ∈ B--5
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -5) AND (<= low high) AND (vector-ref a-voi (sub1 low)) ∈ B--5
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM -4)
                                 ;; INV: (= BUCKET-NUM -4) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -4) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--4)
                                   ;; INV: (= BUCKET-NUM -4) AND (<= low high) AND (vector-ref a-voi low) ∈ B--4
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -4) AND (vector-ref a-voi (sub1 low)) ∈ B--4
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM -3)
                                 ;; INV: (= BUCKET-NUM -3) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -3) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--3)
                                   ;; INV: (= BUCKET-NUM -3) AND (<= low high) AND (vector-ref a-voi low) ∈ B--3
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -3) AND (vector-ref a-voi (sub1 low)) ∈ B--3
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM -2)
                                 ;; INV: (= BUCKET-NUM -2) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -2) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--2)
                                   ;; INV: (= BUCKET-NUM -2) AND (<= low high) AND (vector-ref a-voi low) ∈ B--2
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -2) AND (vector-ref a-voi (sub1 low)) ∈ B--2
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM -1)
                                 ;; INV: (= BUCKET-NUM -1) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM -1) AND (<= low high)
                                   (add! (vector-ref a-voi low) B--1)
                                   ;; INV: (= BUCKET-NUM -1) AND (<= low high) AND (vector-ref a-voi low) ∈ B--1
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM -1) AND (vector-ref a-voi (sub1 low)) ∈ B--1
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 0)
                                 ;; INV: (= BUCKET-NUM 0) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 0) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-0)
                                   ;; INV: (= BUCKET-NUM 0) AND (<= low high) AND (vector-ref a-voi low) ∈ B-0
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 0) AND (vector-ref a-voi (sub1 low)) ∈ B-0
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 1)
                                 ;; INV: (= BUCKET-NUM 1) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 1) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-1)
                                   ;; INV: (= BUCKET-NUM 1) AND (<= low high) AND (vector-ref a-voi low) ∈ B-1
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 1) AND (vector-ref a-voi (sub1 low)) ∈ B-1
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 2)
                                 ;; INV: (= BUCKET-NUM 2) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 2) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-2)
                                   ;; INV: (= BUCKET-NUM 2) AND (<= low high) AND (vector-ref a-voi low) ∈ B-2
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 2) AND (vector-ref a-voi (sub1 low)) ∈ B-2
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 3)
                                 ;; INV: (= BUCKET-NUM 3) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 3) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-3)
                                   ;; INV: (= BUCKET-NUM 3) AND (<= low high) AND (vector-ref a-voi low) ∈ B-3
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 3) AND (vector-ref a-voi (sub1 low)) ∈ B-3
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 4)
                                 ;; INV: (= BUCKET-NUM 4) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 4) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-4)
                                   ;; INV: (= BUCKET-NUM 4) AND (<= low high) AND (vector-ref a-voi low) ∈ B-4
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 4) AND (vector-ref a-voi (sub1 low)) ∈ B-4
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 5)
                                 ;; INV: (= BUCKET-NUM 5) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 5) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-5)
                                   ;; INV: (= BUCKET-NUM 5) AND (<= low high) AND (vector-ref a-voi low) ∈ B-5
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 5) AND (vector-ref a-voi (sub1 low)) ∈ B-5
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 6)
                                 ;; INV: (= BUCKET-NUM 6) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 6) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-6)
                                   ;; INV: (= BUCKET-NUM 6) AND (<= low high) AND (vector-ref a-voi low) ∈ B-6
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 6) AND (vector-ref a-voi (sub1 low)) ∈ B-6
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 7)
                                 ;; INV: (= BUCKET-NUM 7) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 7) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-7)
                                   ;; INV: (= BUCKET-NUM 7) AND (<= low high) AND (vector-ref a-voi low) ∈ B-7
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 7) AND (vector-ref a-voi (sub1 low)) ∈ B-7
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 8)
                                 ;; INV: (= BUCKET-NUM 8) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 8) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-8)
                                   ;; INV: (= BUCKET-NUM 8) AND (<= low high) AND (vector-ref a-voi low) ∈ B-8
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 8) AND (vector-ref a-voi (sub1 low)) ∈ B-8
                                   (radix-sort-helper! a-voi))]
                                [(= BUCKET-NUM 9)
                                 ;; INV: (= BUCKET-NUM 9) AND (<= low high)
                                 (begin
                                   ;; INV: (= BUCKET-NUM 9) AND (<= low high)
                                   (add! (vector-ref a-voi low) B-9)
                                   ;; INV: (= BUCKET-NUM 9) AND (<= low high) AND (vector-ref a-voi low) ∈ B-9
                                   (set! low (add1 low))
                                   ;; INV: (= BUCKET-NUM 9) AND (vector-ref a-voi (sub1 low)) ∈ B-9
                                   (radix-sort-helper! a-voi))])))))

              ;;-> (void)
              ;;Purpose: Initialize buckets
              (define (init-buckets)
                (begin
                  (init-bucket! B--9 a-voi)
                  (init-bucket! B--8 a-voi)
                  (init-bucket! B--7 a-voi)
                  (init-bucket! B--6 a-voi)
                  (init-bucket! B--5 a-voi)
                  (init-bucket! B--4 a-voi)
                  (init-bucket! B--3 a-voi)
                  (init-bucket! B--2 a-voi)
                  (init-bucket! B--1 a-voi)
                  (init-bucket! B-0 a-voi)
                  (init-bucket! B-1 a-voi)
                  (init-bucket! B-2 a-voi)
                  (init-bucket! B-3 a-voi)
                  (init-bucket! B-4 a-voi)
                  (init-bucket! B-5 a-voi)
                  (init-bucket! B-6 a-voi)
                  (init-bucket! B-7 a-voi)
                  (init-bucket! B-8 a-voi)
                  (init-bucket! B-9 a-voi)))
                  
              ]
        (begin
          (set! low 0)
          (set! high (sub1(vector-length a-voi)))
          (set! expo-val 0)
          (init-buckets)
          (radix-sort-helper! a-voi)))))
      


;;Tests

(check-expect (begin
                (radix-sort! V0)
                V0)
              (vector))

(check-expect (begin
                (radix-sort! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (radix-sort! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46 60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (radix-sort! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (radix-sort! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (radix-sort! V5)
                V5)
              (vector -1008 -19 -1 8 19 32))


;                                                                 
;                                                                 
;                        ;  ;                                     
;   ;;;;;;              ;;                  ;;; ;                 
;    ;    ;              ;                 ;   ;;              ;  
;    ;    ;              ;                 ;    ;              ;  
;    ;    ;    ;;;    ;; ;  ; ;;; ;;;      ;        ;;;   ; ;;;;;;
;    ;;;;;    ;   ;  ;  ;; ;;  ;   ;        ;;     ;   ; ;;;   ;  
;    ;  ;         ;  ;   ;  ;   ; ;           ;;   ;   ;  ;    ;  
;    ;   ;     ;;;;  ;   ;  ;    ;    ;;;       ;  ;   ;  ;    ;  
;    ;   ;    ;   ;  ;   ;  ;   ; ;        ;    ;  ;   ;  ;    ;  
;    ;    ;   ;   ;  ;  ;;; ;  ;   ;       ;;   ;  ;   ;  ;    ; ;
;   ;;;   ;;;  ;;;;;  ;; ; ;;;;;; ;;;      ; ;;;    ;;;  ;;;   ;; 
;                                                                 
;                                                                 
;                                                                 
;                                                                 


;;a-voi -> (void)
;;Purpose: Sort a given vector in non-decreasing order
;;Termination Argument:
;;If the vector length is less than 2, then there is no recursion. Otherwise, during each recursive call of the function the expo-val
;;is incremented by 1. This continues until the expo-val is equal to MAX-DIGITS, the amount of times the fuction is recursively called.
;;When expo-val is equal to MAX-DIGITS, the function returns (void). Thus, terminating.
(define (radix-sort-with-while! a-voi)
  (if (< (vector-length a-voi) 2)
      (void)
      (local [
              ;; number
              ;; Purpose: Amound of digits in the largest integer in the vector
              (define MAX-DIGITS (num-of-digits a-voi))

              ;; number
              ;; Purpose: The low of the unprocessed interval 
              (define low (void))

              ;; number
              ;; Purpose: The high of the unprocessed interval
              (define high (void))

              ;; number
              ;; Purpose: The exponent that 10 is exponated to in order to find the number at a digit place
              (define expo-val (void))

              (define B--9 (make-bucket))
              (define B--8 (make-bucket))
              (define B--7 (make-bucket))
              (define B--6 (make-bucket))
              (define B--5 (make-bucket))
              (define B--4 (make-bucket))
              (define B--3 (make-bucket))
              (define B--2 (make-bucket))
              (define B--1 (make-bucket))
              
              (define B-0 (make-bucket))
              (define B-1 (make-bucket))
              (define B-2 (make-bucket))
              (define B-3 (make-bucket))
              (define B-4 (make-bucket))
              (define B-5 (make-bucket))
              (define B-6 (make-bucket))
              (define B-7 (make-bucket))
              (define B-8 (make-bucket))
              (define B-9 (make-bucket))

              ;;a-voi -> (void)
              ;;Purpose: Sort a given vector in non-decreasing order
              ;;Effect: If (not (= expo-val MAX-DIGITS)) AND low > high, then each bucket is dumped into the vector
              ;;        AND expo-val is incremented by one AND low is set to 0.
              ;;        If (not (= expo-val MAX-DIGITS)) AND low < high, then (vector-ref a-voi low) is added to the correct
              ;;        bucket AND low is incremented by 1.
              (define (radix-sort-helper! a-voi)
                ;; INV: (vector-length a-voi) > 1
                (while (< expo-val MAX-DIGITS)
                       (if (> low high)
                           (local [(define LENGTH-OF-V (vector-length a-voi))
                                   (define size-9 (size B--9))
                                   (define size-8 (+ size-9 (size B--8)))
                                   (define size-7 (+ size-8 (size B--7)))
                                   (define size-6 (+ size-7 (size B--6)))
                                   (define size-5 (+ size-6 (size B--5)))
                                   (define size-4 (+ size-5 (size B--4)))
                                   (define size-3 (+ size-4 (size B--3)))
                                   (define size-2 (+ size-3 (size B--2)))                             
                                   (define size-1 (+ size-2 (size B--1)))
                                
                                   (define size0 (+ size-1 (size B-0)))
                                   (define size1 (+ size0 (size B-1)))
                                   (define size2 (+ size1 (size B-2)))
                                   (define size3 (+ size2 (size B-3)))
                                   (define size4 (+ size3 (size B-4)))
                                   (define size5 (+ size4 (size B-5)))
                                   (define size6 (+ size5 (size B-6)))
                                   (define size7 (+ size6 (size B-7)))
                                   (define size8 (+ size7 (size B-8)))]
                             ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1
                             (begin
                               ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1
                            (dump! a-voi 0 B--9)
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0
                            (if (>= size-9 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0
                                (dump! a-voi size-9 B--8))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            (if (>= size-8 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                (dump! a-voi size-8 B--7))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0

                            (if (>= size-7 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0
                                (dump! a-voi size-7 B--6))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 1 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0

                            (if (>= size-6 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0
                                (dump! a-voi size-6 B--5))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0

                            (if (>= size-5 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0
                                (dump! a-voi size-5 B--4))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0

                            (if (>= size-4 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                (dump! a-voi size-4 B--3))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0

                            (if (>= size-3 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0
                                (dump! a-voi size-3 B--2))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0

                            (if (>= size-2 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0
                                (dump! a-voi size-2 B--1))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0

                            (if (>= size-1 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0
                                (dump! a-voi size-1 B-0))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                          
                            (if (>= size0 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                (dump! a-voi size0 B-1))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0
                        
                            (if (>= size1 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0
                                (dump! a-voi size1 B-2))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0
                         
                            (if (>= size2 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0
                                (dump! a-voi size2 B-3))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0
                          
                            (if (>= size3 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0
                                (dump! a-voi size3 B-4))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                          
                            (if (>= size4 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                                (dump! a-voi size4 B-5))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0
                          
                            (if (>= size5 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0
                                (dump! a-voi size5 B-6))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0
                          
                            (if (>= size6 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0
                                (dump! a-voi size6 B-7))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0
                         
                            (if (>= size7 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0
                                (dump! a-voi size7 B-8))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                          
                            (if (>= size8 LENGTH-OF-V)
                                (void)
                                ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                                (dump! a-voi size8 B-9))
                            ;; INV: expo-val < MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0
                          

                            (set! expo-val (add1 expo-val))

                            ;; INV: expo-val <= MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0
                            (set! low 0)

                            ;; INV: expo-val <= MAX-DIGITS AND (vector-length a-voi) > 1 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0 AND (= low 0)
                           
                               (radix-sort-helper! a-voi)))
                           (local [(define BUCKET-NUM (remainder (quotient (vector-ref a-voi low) (expt 10 expo-val)) 10))]
                             (cond [(= BUCKET-NUM -9)
                                    ;; INV: (= BUCKET-NUM -9) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -9) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--9)
                                      ;; INV: (= BUCKET-NUM -9) AND (<= low high) AND (vector-ref a-voi low) ∈ B--9
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -9) AND (vector-ref a-voi (sub1 low)) ∈ B--9
                                      )]
                                   [(= BUCKET-NUM -8)
                                    ;; INV: (= BUCKET-NUM -8) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -8) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--8)
                                      ;; INV: (= BUCKET-NUM -8) AND (<= low high) AND (vector-ref a-voi low) ∈ B--8
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -8) AND (vector-ref a-voi (sub1 low)) ∈ B--8
                                      )]
                                   [(= BUCKET-NUM -7)
                                    ;; INV: (= BUCKET-NUM -7) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -7) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--7)
                                      ;; INV: (= BUCKET-NUM -7) AND (<= low high) AND (vector-ref a-voi low) ∈ B--7
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -7) AND (vector-ref a-voi (sub1 low)) ∈ B--7
                                      )]
                                   [(= BUCKET-NUM -6)
                                    ;; INV: (= BUCKET-NUM -6) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -6) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--6)
                                      ;; INV: (= BUCKET-NUM -6) AND (<= low high) AND (vector-ref a-voi low) ∈ B--6
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -6) AND (vector-ref a-voi (sub1 low)) ∈ B--6
                                      )]
                                   [(= BUCKET-NUM -5)
                                    ;; INV: (= BUCKET-NUM -5) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -5) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--5)
                                      ;; INV: (= BUCKET-NUM -5) AND (<= low high) AND (vector-ref a-voi low) ∈ B--5
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -5) AND (<= low high) AND (vector-ref a-voi (sub1 low)) ∈ B--5
                                      )]
                                   [(= BUCKET-NUM -4)
                                    ;; INV: (= BUCKET-NUM -4) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -4) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--4)
                                      ;; INV: (= BUCKET-NUM -4) AND (<= low high) AND (vector-ref a-voi low) ∈ B--4
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -4) AND (vector-ref a-voi (sub1 low)) ∈ B--4
                                      )]
                                   [(= BUCKET-NUM -3)
                                    ;; INV: (= BUCKET-NUM -3) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -3) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--3)
                                      ;; INV: (= BUCKET-NUM -3) AND (<= low high) AND (vector-ref a-voi low) ∈ B--3
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -3) AND (vector-ref a-voi (sub1 low)) ∈ B--3
                                      )]
                                   [(= BUCKET-NUM -2)
                                    ;; INV: (= BUCKET-NUM -2) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -2) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--2)
                                      ;; INV: (= BUCKET-NUM -2) AND (<= low high) AND (vector-ref a-voi low) ∈ B--2
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -2) AND (vector-ref a-voi (sub1 low)) ∈ B--2
                                      )]
                                   [(= BUCKET-NUM -1)
                                    ;; INV: (= BUCKET-NUM -1) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM -1) AND (<= low high)
                                      (add! (vector-ref a-voi low) B--1)
                                      ;; INV: (= BUCKET-NUM -1) AND (<= low high) AND (vector-ref a-voi low) ∈ B--1
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM -1) AND (vector-ref a-voi (sub1 low)) ∈ B--1
                                      )]
                                   [(= BUCKET-NUM 0)
                                    ;; INV: (= BUCKET-NUM 0) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 0) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-0)
                                      ;; INV: (= BUCKET-NUM 0) AND (<= low high) AND (vector-ref a-voi low) ∈ B-0
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 0) AND (vector-ref a-voi (sub1 low)) ∈ B-0
                                      )]
                                   [(= BUCKET-NUM 1)
                                    ;; INV: (= BUCKET-NUM 1) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 1) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-1)
                                      ;; INV: (= BUCKET-NUM 1) AND (<= low high) AND (vector-ref a-voi low) ∈ B-1
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 1) AND (vector-ref a-voi (sub1 low)) ∈ B-1
                                      )]
                                   [(= BUCKET-NUM 2)
                                    ;; INV: (= BUCKET-NUM 2) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 2) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-2)
                                      ;; INV: (= BUCKET-NUM 2) AND (<= low high) AND (vector-ref a-voi low) ∈ B-2
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 2) AND (vector-ref a-voi (sub1 low)) ∈ B-2
                                      )]
                                   [(= BUCKET-NUM 3)
                                    ;; INV: (= BUCKET-NUM 3) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 3) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-3)
                                      ;; INV: (= BUCKET-NUM 3) AND (<= low high) AND (vector-ref a-voi low) ∈ B-3
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 3) AND (vector-ref a-voi (sub1 low)) ∈ B-3
                                      )]
                                   [(= BUCKET-NUM 4)
                                    ;; INV: (= BUCKET-NUM 4) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 4) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-4)
                                      ;; INV: (= BUCKET-NUM 4) AND (<= low high) AND (vector-ref a-voi low) ∈ B-4
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 4) AND (vector-ref a-voi (sub1 low)) ∈ B-4
                                      )]
                                   [(= BUCKET-NUM 5)
                                    ;; INV: (= BUCKET-NUM 5) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 5) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-5)
                                      ;; INV: (= BUCKET-NUM 5) AND (<= low high) AND (vector-ref a-voi low) ∈ B-5
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 5) AND (vector-ref a-voi (sub1 low)) ∈ B-5
                                      )]
                                   [(= BUCKET-NUM 6)
                                    ;; INV: (= BUCKET-NUM 6) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 6) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-6)
                                      ;; INV: (= BUCKET-NUM 6) AND (<= low high) AND (vector-ref a-voi low) ∈ B-6
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 6) AND (vector-ref a-voi (sub1 low)) ∈ B-6
                                      )]
                                   [(= BUCKET-NUM 7)
                                    ;; INV: (= BUCKET-NUM 7) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 7) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-7)
                                      ;; INV: (= BUCKET-NUM 7) AND (<= low high) AND (vector-ref a-voi low) ∈ B-7
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 7) AND (vector-ref a-voi (sub1 low)) ∈ B-7
                                      (radix-sort-helper! a-voi))]
                                   [(= BUCKET-NUM 8)
                                    ;; INV: (= BUCKET-NUM 8) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 8) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-8)
                                      ;; INV: (= BUCKET-NUM 8) AND (<= low high) AND (vector-ref a-voi low) ∈ B-8
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 8) AND (vector-ref a-voi (sub1 low)) ∈ B-8
                                      )]
                                   [(= BUCKET-NUM 9)
                                    ;; INV: (= BUCKET-NUM 9) AND (<= low high)
                                    (begin
                                      ;; INV: (= BUCKET-NUM 9) AND (<= low high)
                                      (add! (vector-ref a-voi low) B-9)
                                      ;; INV: (= BUCKET-NUM 9) AND (<= low high) AND (vector-ref a-voi low) ∈ B-9
                                      (set! low (add1 low))
                                      ;; INV: (= BUCKET-NUM 9) AND (vector-ref a-voi (sub1 low)) ∈ B-9
                                      )])))))

              ;;-> (void)
              ;;Purpose: Initialize buckets
              (define (init-buckets)
                (begin
                  (init-bucket! B--9 a-voi)
                  (init-bucket! B--8 a-voi)
                  (init-bucket! B--7 a-voi)
                  (init-bucket! B--6 a-voi)
                  (init-bucket! B--5 a-voi)
                  (init-bucket! B--4 a-voi)
                  (init-bucket! B--3 a-voi)
                  (init-bucket! B--2 a-voi)
                  (init-bucket! B--1 a-voi)
                  (init-bucket! B-0 a-voi)
                  (init-bucket! B-1 a-voi)
                  (init-bucket! B-2 a-voi)
                  (init-bucket! B-3 a-voi)
                  (init-bucket! B-4 a-voi)
                  (init-bucket! B-5 a-voi)
                  (init-bucket! B-6 a-voi)
                  (init-bucket! B-7 a-voi)
                  (init-bucket! B-8 a-voi)
                  (init-bucket! B-9 a-voi)))
                  
              ]
        (begin
          (set! low 0)
          (set! high (sub1(vector-length a-voi)))
          (set! expo-val 0)
          (init-buckets)
          (radix-sort-helper! a-voi)))))

;;Tests

(check-expect (begin
                (radix-sort-with-while! V0)
                V0)
              (vector))

(check-expect (begin
                (radix-sort-with-while! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (radix-sort-with-while! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46 60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (radix-sort-with-while! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (radix-sort-with-while! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (radix-sort-with-while! V5)
                V5)
              (vector -1008 -19 -1 8 19 32))


;                                                                                  
;                                                                                 
;                                     ;                                            
;   ;;;                                                      ;;; ;                 
;    ;                            ;                         ;   ;;              ;  
;    ;                            ;                         ;    ;              ;  
;    ;   ; ;;    ;;;   ;;;   ; ;;;;;; ;   ;;;   ; ;;        ;        ;;;   ; ;;;;;;
;    ;  ;;;  ;  ;  ;  ;   ; ;;;   ;  ;;  ;   ; ;;;  ;        ;;     ;   ; ;;;   ;  
;    ;   ;   ;  ;     ;;;;;  ;    ;   ;  ;   ;  ;   ;          ;;   ;   ;  ;    ;  
;    ;   ;   ;   ;;   ;      ;    ;   ;  ;   ;  ;   ;  ;;;       ;  ;   ;  ;    ;  
;    ;   ;   ;     ;  ;      ;    ;   ;  ;   ;  ;   ;       ;    ;  ;   ;  ;    ;  
;    ;   ;   ;  ;  ;  ;   ;  ;    ; ; ;  ;   ;  ;   ;       ;;   ;  ;   ;  ;    ; ;
;   ;;; ;;; ;;; ;;;    ;;;  ;;;   ;; ;;;  ;;;  ;;; ;;;      ; ;;;    ;;;  ;;;   ;; 
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  


;; -> (void)
;; Purpose: To sort a given vector in non-decreasing order
(define (insertion-sort! a-voi)
  (local [;;number
          ;;Purpose: Low index of the unprocessed vector interval being sorted in non-decreasing order
          (define low (void))
          ;;number
          ;;Purpose: High index of the vector interval
          (define high (void))

          ;; -> (void)
          ;; Purpose: To sort the vector in non-decreasing order
          ;; INVARIANT:
          ;; [V[0]... V[(sub1 low)] + [V[low]...V[high]] = (vector-length a-voi)
          ;; AND (= high (sub1 (vector-length a-voi)))
          ;; Effect: insert-sort-helper! is ran AND low is incremented by 1 with each recursive call
          ;;Termination Argument:
          ;;If high is less than low, the function terminates. Otherwise, low is incremented by one during
          ;;each recursive call until low is greater than high. Thus, terminating.
          (define (insert-sort!)
            (if (or (< (vector-length a-voi) 2) (< high low))
                (void)
                (local [;;number
                        ;;Purpose: To hold the value of (vector-ref a-voi low)
                        (define low-val (void))

                        ;;number
                        ;;Purpose: The current index being compared
                        (define cur-idx (void))

                        ;; -> (void)
                        ;; Purpose: To sort the low and sub1 low vector-ref value in non-decreasing order
                        ;; INV:
                        ;; (>= (vector-length a-voi) 2) AND (< high low) AND (= high (sub1 (vector-length a-voi)))
                        ;; Effect: low-val is set to (vector-ref a-voi low) AND a-voi[low] is set to (vector-ref a-voi (sub1 low))
                        ;;         AND a-voi[(sub1 low)] is set to low-val and cur-idx is set to (sub1 low).
                        ;; Termination Argument:
                        ;; If (>= (vector-ref a-voi low) (vector-ref a-voi (sub1 low))), then the function terminates,
                        ;; Otherwise, it swaps the values at (vector-ref a-voi low) and (vector-ref a-voi (sub1 low)),
                        ;; then calls another function which terminates. Therefore, since the condition of termination
                        ;; is met, the function terminates.
                        (define (insert-sort-helper!) 
                          (if (or (= low 0) (>= (vector-ref a-voi low) (vector-ref a-voi (sub1 low))))
                              (void)
                              (local [;; -> (void)
                                      ;; Purpose: To sort a number with the numbers before it in the vector in non-decreasing order
                                      ;; INVARIANT:
                                      ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high 1 AND
                                      ;;(> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                      ;; AND curr-idx-val = (vector-ref a-voi (add1 cur-idx))
                                      ;; Effect: curr-idx-val is set to (vector-ref a-voi cur-idx) AND a-voi[cur-idx] is set to
                                      ;;         (vector-ref a-voi (sub1 cur-idx)) AND a-voi[(sub1 cur-idx)] is set to curr-idx-val
                                      ;;         AND cur-idx is decremented by 1 during each recursive call.
                                      ;; Termination Argument:
                                      ;; If low is < 2, 1 > cur-idx, or (> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                      ;; the function terminates. Otherwise, (vector-ref a-voi cur-idx)
                                      ;; and (vector-ref a-voi (sub1 cur-idx)) are swapped and cur-idx is decremented by 1 during
                                      ;; each recursive call. Therefore, the function will eventually terminated because cur-idx will be
                                      ;; less than 1.
                                      (define (sort-behind)
                                        (if (or (< low 2)
                                                (> 1 cur-idx)
                                                (> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx))))
                                            (void)
                                            (local [;;number
                                                    ;;Purpose: Value of (vector-ref a-voi cur-idx)
                                                    (define curr-idx-val (void))]
                                              (begin
                                                ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 1 AND
                                                ;;(< (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                                ;; AND curr-idx-val = (vector-ref a-voi (add1 cur-idx))
                                              
                                                (set! curr-idx-val (vector-ref a-voi cur-idx))
                                              
                                                ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 1 AND
                                                ;;(< (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                                ;; AND curr-idx-val = (vector-ref a-voi cur-idx)
                                              
                                                (vector-set! a-voi cur-idx (vector-ref a-voi (sub1 cur-idx)))
                                              
                                                ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 1 AND
                                                ;;(= (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                                ;; AND curr-idx-val = (vector-ref a-voi cur-idx)
                                              
                                                (vector-set! a-voi (sub1 cur-idx) curr-idx-val)
                                              
                                                ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 1 AND
                                                ;;(> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                                ;; AND curr-idx-val = (vector-ref a-voi cur-idx)
                                              
                                                (set! cur-idx (sub1 cur-idx))

                                                ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 0 AND
                                                ;;(> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                                ;; AND curr-idx-val = (vector-ref a-voi cur-idx)
                                              
                                                (sort-behind)))))
                                              
                                      ]
                                (begin
                                  ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                  ;; AND (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                  (set! low-val (vector-ref a-voi low))
                                  ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                  ;; AND (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                  (vector-set! a-voi low (vector-ref a-voi (sub1 low)))
                                  ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                  ;; AND (= (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                  (vector-set! a-voi (sub1 low) low-val)
                                  ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                  ;; AND (> (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                  (set! cur-idx (sub1 low))
                                  ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                  ;; AND (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                  (sort-behind)))))]
                  (begin
                    ;; (> (vector-length a-voi) 2) AND (< low high)
                    (insert-sort-helper!)
                    ;; (> (vector-length a-voi) 2) AND (< low high) AND (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                    (set! low (add1 low))
                    ;; (> (vector-length a-voi) 2) AND (<= low high) AND (< (vector-ref a-voi (sub1 low)) (vector-ref a-voi (- low 2)))
                    (insert-sort!)))))]
    (begin
      (set! high (sub1 (vector-length a-voi)))
      (set! low 0)
      (insert-sort!)
       #|
INV: Low > high AND voi is sorted in non-decreasing order
|#)))


;;Tests

(check-expect (begin
                (insertion-sort! V0)
                V0)
              (vector))

(check-expect (begin
                (insertion-sort! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (insertion-sort! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46 60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (insertion-sort! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (insertion-sort! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (insertion-sort! V5)
                V5)
              (vector -1008 -19 -1 8 19 32))

;                                                                                  
;                                                                                 
;                                     ;                                            
;   ;;;                                                      ;;; ;                 
;    ;                            ;                         ;   ;;              ;  
;    ;                            ;                         ;    ;              ;  
;    ;   ; ;;    ;;;   ;;;   ; ;;;;;; ;   ;;;   ; ;;        ;        ;;;   ; ;;;;;;
;    ;  ;;;  ;  ;  ;  ;   ; ;;;   ;  ;;  ;   ; ;;;  ;        ;;     ;   ; ;;;   ;  
;    ;   ;   ;  ;     ;;;;;  ;    ;   ;  ;   ;  ;   ;          ;;   ;   ;  ;    ;  
;    ;   ;   ;   ;;   ;      ;    ;   ;  ;   ;  ;   ;  ;;;       ;  ;   ;  ;    ;  
;    ;   ;   ;     ;  ;      ;    ;   ;  ;   ;  ;   ;       ;    ;  ;   ;  ;    ;  
;    ;   ;   ;  ;  ;  ;   ;  ;    ; ; ;  ;   ;  ;   ;       ;;   ;  ;   ;  ;    ; ;
;   ;;; ;;; ;;; ;;;    ;;;  ;;;   ;; ;;;  ;;;  ;;; ;;;      ; ;;;    ;;;  ;;;   ;; 
;                                                                                  
;                                                                                  
;                                                                                  
;                                                          


;; -> (void)
;; Purpose: To sort a given vector in non-decreasing order
(define (insertion-sort-with-while! a-voi)
  (if (< (vector-length a-voi) 2)
      (void)
      (local [;;number
              ;;Purpose: Low index of the unprocessed vector interval being sorted in non-decreasing order
              (define low (void))
              ;;number
              ;;Purpose: High index of the vector interval
              (define high (void))

              ;; -> (void)
              ;; Purpose: To sort the vector in non-decreasing order
              ;; INVARIANT:
              ;; [V[0]... V[(sub1 low)] + [V[low]...V[high]] = (vector-length a-voi)
              ;; AND (= high (sub1 (vector-length a-voi)))
              ;;Termination Argument:
              ;;If high is less than low, the function terminates. Otherwise, low is incremented by one during
              ;;each recursive call until low is greater than high. Thus, terminating.
              (define (insert-sort!)
                (local [;;number
                        ;;Purpose: To hold the value of (vector-ref a-voi low)
                        (define low-val (void))

                        ;;number
                        ;;Purpose: The current index being compared
                        (define cur-idx (void))
                        ;; -> (void)
                        ;; Purpose: To sort the low and sub1 low vector-ref value in non-decreasing order
                        ;; INV:
                        ;; (> (vector-length a-voi) 1) AND (< high low)
                        ;; Termination Argument:
                        ;; If (>= (vector-ref a-voi low) (vector-ref a-voi (sub1 low))), then the function terminates,
                        ;; Otherwise, it swaps the values at (vector-ref a-voi low) and (vector-ref a-voi (sub1 low)),
                        ;; then calls another function which terminates. Therefore, since the condition of termination
                        ;; is met, the function terminates.
                        (define (insert-sort-helper!)
                          (local [;; -> (void)
                                  ;; Purpose: To sort a number with the numbers before it in the vector in non-decreasing order
                                  ;; INVARIANT:
                                  ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high 1 AND
                                  ;;(> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                  ;; AND curr-idx-val = (vector-ref a-voi (add1 cur-idx))
                                  ;; Termination Argument:
                                  ;; If low is < 2, 1 > cur-idx, or (> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                  ;; the function terminates. Otherwise, (vector-ref a-voi cur-idx)
                                  ;; and (vector-ref a-voi (sub1 cur-idx)) are swapped and cur-idx is decremented by 1 during
                                  ;; each recursive call. Therefore, the function will eventually terminated because cur-idx will be
                                  ;; less than 1.
                                  (define (sort-behind)
                                    (local [(define curr-idx-val (void))]

                                      (while (and (> low 1)
                                                  (> cur-idx 0)
                                                  (< (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx))))

                                             ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 1 AND
                                             ;;(< (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                             ;; AND curr-idx-val = (vector-ref a-voi (add1 cur-idx))
                                              
                                             (set! curr-idx-val (vector-ref a-voi cur-idx))
                                              
                                             ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 1 AND
                                             ;;(< (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                             ;; AND curr-idx-val = (vector-ref a-voi cur-idx)
                                              
                                             (vector-set! a-voi cur-idx (vector-ref a-voi (sub1 cur-idx)))
                                              
                                             ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 1 AND
                                             ;;(= (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                             ;; AND curr-idx-val = (vector-ref a-voi cur-idx)
                                              
                                             (vector-set! a-voi (sub1 cur-idx) curr-idx-val)
                                              
                                             ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 1 AND
                                             ;;(> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                             ;; AND curr-idx-val = (vector-ref a-voi cur-idx)
                                              
                                             (set! cur-idx (sub1 cur-idx))

                                             ;; high = (sub1 (vector-length a-voi)) AND 2 <= low <= high AND cur-idx >= 0 AND
                                             ;;(> (vector-ref a-voi cur-idx) (vector-ref a-voi (sub1 cur-idx)))
                                             ;; AND curr-idx-val = (vector-ref a-voi cur-idx)
                                             )))]

                            (while (and (> low 0)
                                        (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low))))
                                   ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                   ;; AND (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                   (set! low-val (vector-ref a-voi low))
                                   ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                   ;; AND (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                   (vector-set! a-voi low (vector-ref a-voi (sub1 low)))
                                   ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                   ;; AND (= (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                   (vector-set! a-voi (sub1 low) low-val)
                                   ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                   ;; AND (> (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                   (set! cur-idx (sub1 low))
                                   ;; (> (vector-length a-voi) 1) AND (< high low) AND (> low 0)
                                   ;; AND (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                                   (sort-behind))))]
                  
                  (while (<= low high)
                         ;; (> (vector-length a-voi) 2) AND (< low high)
                         (insert-sort-helper!)
                         ;; (> (vector-length a-voi) 2) AND (< low high) AND (< (vector-ref a-voi low) (vector-ref a-voi (sub1 low)))
                         (set! low (add1 low))
                         ;; (> (vector-length a-voi) 2) AND (<= low high) AND (< (vector-ref a-voi (sub1 low)) (vector-ref a-voi (- low 2)))
                         )))]
        (begin
          (set! high (sub1 (vector-length a-voi)))
          (set! low 0)
          (insert-sort!)
          #|
INV: Low > high AND voi is sorted in non-decreasing order
|#))))

;;Tests

(check-expect (begin
                (insertion-sort-with-while! V0)
                V0)
              (vector))

(check-expect (begin
                (insertion-sort-with-while! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (insertion-sort-with-while! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46 60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (insertion-sort-with-while! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (insertion-sort-with-while! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (insertion-sort-with-while! V5)
                V5)
              (vector -1008 -19 -1 8 19 32))

;                                                                  
;                                                                  
;                     ;         ;                                  
;     ;;;;;                    ;;            ;;; ;                 
;    ;     ;                    ;           ;   ;;              ;  
;   ;       ;                   ;           ;    ;              ;  
;   ;       ; ;;  ;;  ;   ;;;   ;  ;;;      ;        ;;;   ; ;;;;;;
;   ;       ;  ;   ; ;;  ;   ;  ;  ;         ;;     ;   ; ;;;   ;  
;   ;       ;  ;   ;  ;  ;      ; ;            ;;   ;   ;  ;    ;  
;   ;       ;  ;   ;  ;  ;      ;;;    ;;;       ;  ;   ;  ;    ;  
;   ;       ;  ;   ;  ;  ;      ;  ;        ;    ;  ;   ;  ;    ;  
;    ;     ;   ;  ;;; ;  ;   ;  ;   ;       ;;   ;  ;   ;  ;    ; ;
;     ;;;;;     ;; ; ;;;  ;;;  ;;; ;;;      ; ;;;    ;;;  ;;;   ;; 
;       ;                                                          
;        ;;                                                        
;          ;;                                                      
;                                                                  


;; (vectorof number) --> (void)
;; Purpose: Sort the given vector in nondecreasing order
;; Effect: Given vector elements are rearranged in place
(define (qs-in-place! V)
  (local [;; natnum natnum --> (void)
          ;; Purpose: Swap the elements at the given indices
          ;; Effect: V is mutated by swapping elements at given indices
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))

          ;; number [int int] --> natnum
          ;; Purpose: Find first index <= high, i, such that V[i] <= given pivot
          ;; Assumption: V[low] = pivot
          (define (find<= pivot low high)
            (if (or (> low high)
                    (<= (vector-ref V high) pivot))
                high
                (find<= pivot low (sub1 high))))

          ;; number [int int] --> natnum
          ;; Purpose: Find first index >= low, i, such that V[i] > given pivot
          ;;          if it exists. Otherwise, return high + 1
          (define (find> pivot low high)
            (if (or (> low high)
                    (> (vector-ref V low) pivot))
                low
                (find> pivot (add1 low) high)))
          
          ;; number [int int] --> natnum
          ;; Purpose: Return the position of the pivot in the sorted V
          ;; How: The smallest index of a vector element > pivot and the largest
          ;;      index of an element <= to the pivot are found. If they form an
          ;;      empty vector interval the largest index of an element <= to the
          ;;      pivot is returned as the position of the pivot in the sorted
          ;;      vector. Otherwise, the two indexed values are swapped and the
          ;;      partitioning process continues with the vector interval between
          ;;      the two indices.
          ;; Effect: V's elements are rearranged so that all elements before and including
          ;;         the pivot position in sorted V are <= to the pivot and all elements
          ;;         after the pivot position in sorted V are > to the pivot
          (define (partition! pivot low high)
            (local [(define first>pivot (find> pivot low high))
                    (define first<=pivot (find<= pivot low high))]
              (if (> first>pivot first<=pivot)
                  first<=pivot
                  (begin
                    (swap! first>pivot first<=pivot)
                    (partition! pivot
                                first>pivot
                                first<=pivot)))))
          ;; Termination Argument
          ;; Every recursive call is made with a smaller vector interval
          ;; that does not contain the beginning numbers <= to the pivot
          ;; and the ending numbers > pivot. Eventually, the recursive call
          ;; is made with an empty vector interval and the function halts.
          

          ;; [int int] --> (void)
          ;; Purpose: Sort V's elements in the given vector interval in nondecreasing order
          ;; How: The vector is partitioned in two. The first element is placed in the vector
          ;;      position between the elements <= to it and the elements > than it. The
          ;;      The vector intervals for the two parts of the partition are recursively sorted
          ;; Effect: Vector elements in the given vector interval are sorted in place
          (define (qs-aux! low high)
            (if (> low high)
                (void)
                (local [(define pivot (vector-ref V low))
                        (define pivot-pos (partition! pivot low high))]
                  (begin
                    (swap! low pivot-pos)
                    (qs-aux! low (sub1 pivot-pos))
                    (qs-aux! (add1 pivot-pos) high)))))
          ;; Termination Argument
          ;; A given nonempty vector interval is divided into two smaller vector
          ;; intervals and these are recursively processed. Eventually, the given
          ;; vector interval is empty and the function halts.
                  
          ]
    (qs-aux! 0 (sub1 (vector-length V)))))

(check-expect (begin
                (qs-in-place! V0)
                V0)
              (vector))

(check-expect (begin
                (qs-in-place! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (qs-in-place! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46 60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (qs-in-place! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (qs-in-place! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))


#|
A heap is either:
  1. empty
  2. (list number heap heap), where the number is
     greater than the roots, if any, of the two subheaps

Template for functions on a heap

;; Sample heaps
(define HEAP0 '())
(define HEAP1 ...)
     ...
  
;; heap ... --> ...
;; Purpose:
(define (f-on-heap a-heap ...)
  (if (empty? a-heap)
      ...
      ...(first a-heap)...
         (f-on-heap (second a-heap)...
         (f-on-heap (third a-heap)...))

;; Sample expressions for f-on-heap
(define HEAP0-VAL ...)
(define HEAP1-VAL ...)
     ...

;; Tests using sample computations for f-on-heap
(check-expect (f-on-heap HEAP0 ...) HEAP0-VAL)
(check-expect (f-on-heap HEAP1 ...) HEAP1-VAL)
     ...

;; Tests using sample values for f-on-heap
(check-expect (f-on-heap ... ...) ...)
     ...

|#


;                                                               
;                                                               
;                                                               
;   ;;;   ;;;                             ;;; ;                 
;    ;     ;                             ;   ;;              ;  
;    ;     ;                             ;    ;              ;  
;    ;     ;    ;;;    ;;;   ; ;;        ;        ;;;   ; ;;;;;;
;    ;;;;;;;   ;   ;  ;   ; ;;;  ;        ;;     ;   ; ;;;   ;  
;    ;     ;   ;;;;;      ;  ;   ;          ;;   ;   ;  ;    ;  
;    ;     ;   ;       ;;;;  ;   ;  ;;;       ;  ;   ;  ;    ;  
;    ;     ;   ;      ;   ;  ;   ;       ;    ;  ;   ;  ;    ;  
;    ;     ;   ;   ;  ;   ;  ;;  ;       ;;   ;  ;   ;  ;    ; ;
;   ;;;   ;;;   ;;;    ;;;;; ; ;;        ; ;;;    ;;;  ;;;   ;; 
;                            ;                                  
;                            ;                                  
;                           ;;;                                 
;                                                               


;; (vectorof number) --> (void)
;; Purpose: Sort the given vector in nondecreasing order
;; Effect: The given vector's elements are rearranged in nondecreasing order
(define (heap-sort-in-place! V)
  (local [;; natnum natnum --> (void)
          ;; Purpose: Swap the elements at the given indices
          ;; Effect: V is mutated by swapping elements at given indices
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))

          ;; natnum --> natnum
          ;; Purpose: Return the index for the right subheap's root
          (define (right-heap-root parent-index)
            (+ (* 2 parent-index) 2))
                    
          ;; natnum --> natnum
          ;; Purpose: Return the index for the left subheap's root
          (define (left-heap-root parent-index)
            (add1 (* 2 parent-index)))

          ;; natnum --> natnum
          ;; Purpose: Return the parent index in the heap
          ;; Assumption: the given index is in [0..(sub1 (vector-length V))]
          (define (parent i)
            (if (even? i)
                (quotient (sub1 i) 2)
                (quotient i 2)))
                           
          ;; heapify!: [int int] --> (void)
          ;; Purpose: For the given VINTV, make the given vector a heap
          ;; Effect: Reorganizes the vector elements to form a heap rooted at low
          ;; Assumption: The given VINTV is valid for V and low > 0
          (define (heapify! low high)
            (cond [(> low high) (void)]
                  [else
                   (local [(define parent-index (parent high))]
                     (cond [(>= (vector-ref V parent-index) (vector-ref V high))
                            (heapify! low (sub1 high))]
                           [else (begin
                                   (swap! parent-index high)
                                   (trickle-down! high (sub1 (vector-length V)))
                                   (heapify! low (sub1 high)))]))]))

          ;; trickle-down!: [int int] --> (void)
          ;; Purpose: For the given VINTV, re-establish a heap rooted at low
          ;; Effect: Vector elements are rearranged to have a heap rooted at low
          ;; Assumption: V[low+1..high] are all heap roots
          (define (trickle-down! low high)
            (local [(define rc-index (right-heap-root low))
                    (define lc-index (left-heap-root low))]
              (cond [(> lc-index high) (void)] ;; root has no children
                    [(> rc-index high)         ;; root only has a left child
                     (if (<= (vector-ref V lc-index) (vector-ref V low))
                         (void)
                         (begin
                           (swap! low lc-index)
                           (trickle-down! lc-index high)))]
                    [else ;; root has two children
                     (local [(define mc-index (if (>= (vector-ref V lc-index)
                                                      (vector-ref V rc-index))
                                                  lc-index
                                                  rc-index))]
                       (cond [(>= (vector-ref V low) (vector-ref V mc-index))
                              (void)]
                             [else (begin
                                     (swap! low mc-index)
                                     (trickle-down! mc-index high))]))])))
          ;; Termination argument
          ;; A recursive call is only made when V[low] has 1 or 2 children.
          ;; That is, the index of any child must be less than or equal to
          ;; high. Every recursive call is made with an interval formed by
          ;; a child index and high. Eventually, the given vector interval
          ;; becomes empty or V[low} is a heap root and the mutator terminates.
    
          ;; [int int] --> (void)
          ;; Purpose: For the given VINTV, sort the vector elements
          ;; Effect: V's elements in the VINTV are rearranged in non-decreasing order
          ;; Assumption: V is a heap and given VINTV is valid for V
          (define (sorter! low high)
            (cond [(> low high) (void)]
                  [else (begin
                          (swap! low high)
                          (trickle-down! low (sub1 high))
                          (sorter! low (sub1 high)))]))]
    (begin
      (heapify! 1 (sub1 (vector-length V)))
      (sorter! 0 (sub1 (vector-length V))))))

(check-expect (begin
                (heap-sort-in-place! V0)
                V0)
              (vector))

(check-expect (begin
                (heap-sort-in-place! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (heap-sort-in-place! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46 60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (heap-sort-in-place! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (heap-sort-in-place! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))

(define LOSF (list qs-in-place! heap-sort-in-place! radix-sort! radix-sort-with-while! insertion-sort! insertion-sort-with-while!))

;; natnum (listof ((vectorof number) → (void))) → (void)
;; Purpose: Run empirical study with vectors that have lengths
;; that are multiples of 500 in [500..natnum*500]
(define (empirical-study factor lst-of-sorters)
  (local
    [(define NUM-RUNS 5)
     (define V (build-vector (* factor 500) (λ (i) (random 10000000))))
     ;; (vectorof number) natnum (listof ((vectorof number) → (void)))
     ;; → (void)
     ;; Purpose: Time the given in-place sorters using the given vector
     ;; the given number of times
     (define (run-experiments V runs sorters)
       (local [;; (listof ((vectorof number)→(void)))→(void)
               ;; Purpose: Run n experiments
               (define (run sorters)
                 (if (empty? sorters)
                     (void)
                     (local [;; (vectorof number)
                             ;; Purpose: Copy of V to sort
                             (define V1 (build-vector
                                         (vector-length V)
                                         (λ (i) (vector-ref V i))))]
                       (begin
                         (display (format "Sorter ~s: "
                                          (add1 (- (length lst-of-sorters)
                                                   (length sorters)))))
                         (time ((first sorters) V1))
                         (run (rest sorters))))))]
         (if (= runs 0)
             (void)
             (begin
               (display (format " RUN ~s\n" (add1 (- NUM-RUNS runs))))
               (run sorters)
               (run-experiments V (sub1 runs) sorters)))))]
    (if (= factor 0)
        (void)
        (begin
          (display (format "Experiments for length ~s \n" (* factor 500)))
          (run-experiments V NUM-RUNS lst-of-sorters)
          (newline)
          (newline)
          (empirical-study (sub1 factor) lst-of-sorters)))))

;(empirical-study 40 LOSF)

#|
The complexity of our radix-sort is d * 20n where d is the length of the largest digit.
This is because for each call of radix sort, the complexity is 20n, and since radix sort recursively
calls itself as many times as the longest digit, we must multiply the complexity by d. Thus, the complexity
of our radix sort is d * 20n.
|#
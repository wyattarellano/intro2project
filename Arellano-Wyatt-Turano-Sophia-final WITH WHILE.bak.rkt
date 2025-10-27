

;;a-voi -> (void)
;;Purpose: Sort a given vector in non-decreasing order
;;Termination Argument:

(define (radix-sort! a-voi)
  (if (= (vector-length a-voi) 0)
      (void)
      (local [
              ;; number
              ;; Purpose: The largest amounts of digits in the greatest absolute number in the vector 
              (define MAX-DIGITS (num-of-digits a-voi))

              ;; number
              ;; Purpose: The low of the unprocessed interval 
              (define low (void))

              ;; number
              ;; Purpose: The high of the unprocessed interval
              (define high (void))

              ;; number
              ;; Purpose: The exponent that 10 in order to find the number at a digit place
              (define accum (void))

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
              ;;Effect: If (not (= accum MAX-DIGITS)) AND low > high, then each bucket is dumped into the vector
              ;;        AND accum is incremented by one AND low is set to 0.
              ;;        If (not (= accum MAX-DIGITS)) AND low < high, then (vector-ref a-voi low) is added to the correct
              ;;        bucket AND low is incremented by 1.
              (define (radix-sort-helper! a-voi)
                ;; INV: (vector-length a-voi) > 0
                (while (< accum MAX-DIGITS)
                    (if (> low high)
                        (local [
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
                          ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0
                          (begin
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0
                            (dump! a-voi 0 B--9)
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0
                            (if (>= size-9 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0
                                (dump! a-voi size-9 B--8))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            (if (>= size-8 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                (dump! a-voi size-8 B--7))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0

                            (if (>= size-7 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0
                                (dump! a-voi size-7 B--6))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0

                            (if (>= size-6 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0
                                (dump! a-voi size-6 B--5))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0

                            (if (>= size-5 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0
                                (dump! a-voi size-5 B--4))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0

                            (if (>= size-4 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                (dump! a-voi size-4 B--3))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0

                            (if (>= size-3 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0
                                (dump! a-voi size-3 B--2))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0

                            (if (>= size-2 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0
                                (dump! a-voi size-2 B--1))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0

                            (if (>= size-1 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0
                                (dump! a-voi size-1 B-0))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                          
                            (if (>= size0 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                (dump! a-voi size0 B-1))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0
                        
                            (if (>= size1 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0
                                (dump! a-voi size1 B-2))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0
                         
                            (if (>= size2 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0
                                (dump! a-voi size2 B-3))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0
                          
                            (if (>= size3 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0
                                (dump! a-voi size3 B-4))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                          
                            (if (>= size4 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                                (dump! a-voi size4 B-5))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0
                          
                            (if (>= size5 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0
                                (dump! a-voi size5 B-6))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0  AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0
                          
                            (if (>= size6 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0
                                (dump! a-voi size6 B-7))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0
                         
                            (if (>= size7 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0
                                (dump! a-voi size7 B-8))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                          
                            (if (>= size8 (vector-length a-voi))
                                (void)
                                ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                                ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                                ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                                ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                                ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                                (dump! a-voi size8 B-9))
                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0
                          

                            (set! accum (add1 accum))

                            ;; INV: accum <= MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0
                            (set! low 0)

                            ;; INV: accum < MAX-DIGITS AND (vector-length a-voi) > 0 AND (size B--9) = 0 AND (size B--8) = 0
                            ;;      AND (size B--7) = 0 AND (size B--6) = 0 AND (size B--5) = 0 AND (size B--4) = 0
                            ;;      AND (size B--3) = 0 AND (size B--2) = 0 AND (size B--1) = 0 AND (size B-0) = 0
                            ;;      AND (size B-1) = 0 AND (size B-2) = 0 AND (size B-3) = 0 AND (size B-4) = 0
                            ;;      AND (size B-5) = 0 AND (size B-6) = 0 AND (size B-7) = 0 AND (size B-8) = 0
                            ;;      AND (size B-9) = 0 AND (= low 0)
                           
                            ))
                        (local [(define BUCKET-NUM (remainder (quotient (vector-ref a-voi low) (expt 10 accum)) 10))]
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
          (set! accum 0)
          (init-buckets)
          (radix-sort-helper! a-voi)))))
      

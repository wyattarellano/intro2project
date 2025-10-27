#lang htdp/asl

;; A (vectorof integers) (voi) is either:
(define VEC1 (vector 1 10 2 3 4 6))
(define VEC2 (vector 2 3 1 6 99 100 123 243 1 -43 10 1992))
(define VEC3 (vector))


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
          (define (insert-sort!)
            (if (or (= 0 (vector-length a-voi)) (= 1 (vector-length a-voi)) (< high low))
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
                        ;; (> (vector-length a-voi) 1) AND (< high low) AND (= high (sub1 (vector-length a-voi)))
                        ;; Effect: low-val is set to (vector-ref a-voi low) AND a-voi[low] is set to (vector-ref a-voi (sub1 low))
                        ;;         AND a-voi[(sub1 low)] is set to low-val and cur-idx is set to (sub1 low).
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
                                      (define (last-help!)
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
                                              
                                                (last-help!)))))
                                              
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
                                  (last-help!)))))]
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
low > high
AND
voi is sorted in non-decreasing order
|#)))
                              

#lang htdp/asl
(require while)

(define VEC1 (vector 1 2 3 4 5 6 7 8 9 10))
(define VEC2 (vector 2 3 6 99 100 123 243))
(define VEC3 (build-vector 5000 (λ(i) i)))

;;number (vectorof number) -> number
;;Purpose: Return an index of a given number in the given vector. Will return -1 if the number is not in the given vector
;;ASSUMPTION: (vectorof number) is sorted in nondecreasing order
(define (index-in-vector num vector)
  (if (= (vector-length vector) 0)
      -1
      (local [;;natnum
              ;;Purpose: The middle index of each interval
              (define mid-index (void))

              ;;natnum
              ;;Purpose: Low value of interval being search
              (define low (void))

              ;;natnum
              ;;Purpose: High value of interval being searched
              (define high (void))

              ;;natnum
              ;;Purpose: Index of given number
              (define index (void))
              

              ;;->natnum
              ;;Purpose: Find the index of the given number in a vector
              ;;Effect: If the number is less than the mid-index, high gets mutated to (sub1 mid-index).
              ;;        If the number is greater than the mid-index, low gets mutated to (add1 mid-index).
              ;;        If low is greater than high, mid-index gets mutated to -1.
              ;;        Otherwise, mid-index is set to (quotient (+ low high) 2).
              ;;INV: mid-index = (quotient (+ low high) 2)
              ;;Termination Argument:
              ;;The Vector Interval starts as [0...(sub1 (vector-length vector))]. During each loop the function
              ;;checks if the (vector-ref vector index) is not equal to the desired number and the index is not equal
              ;;to -1. If these are false, then the loop will terminate. Otherwise the function checks if the number
              ;;is less than or greater than the (vector-ref vector mid-index). If the number is less than
              ;;(vector-ref vector mid-index), high is changed to (sub1 mid-index), then mid-index is changed
              ;;to (quotient high 2). This means that the interval has become smaller because low has stayed the same
              ;;and high has became a smaller value than it was previously. This means that the function is making progress towards termination
              ;;due to the interval becoming smaller. If the number is greater than (vector-ref vector mid-index), low is changed to
              ;;(add1 mid-index), then mid-index is changed to (quotient (+ low high) 2). This means that the interval has
              ;;became smaller because high has stayed the same and low has became closer to high. This means that the function is
              ;;making progress towards termination due to the interval becoming smaller. Eventually the interval will become
              ;;empty or (= (vector-ref vector index) num). When the interval is empty, the loop will terminate because index will
              ;;equal -1. When the driver (= (vector-ref vector index) num) is true, the number has been found, so the loop will terminate.
              (define (index-finder!)
                (while (and (not (= index -1))
                            (not (= (vector-ref vector index) num)))
                       ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (not (= (vector-ref vector index) num))
                       (cond [(> low high)
                              ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1))
                              ;;      AND (not (= (vector-ref vector index) num))
                              ;;      AND (> low high)
                              (set! index -1)]
                             ;; INV: mid-index = (quotient (+ low high) 2) AND (> low high) AND (= index -1)
                             [(< num (vector-ref vector mid-index))
                              ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                              ;; AND (< low high)
                              (begin
                                (set! high (sub1 mid-index))
                                ;; INV: mid-index = (add1 high) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                ;;      AND (= high (sub1 mid-index))
                                (set! mid-index (quotient high 2))
                                ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                (set! index mid-index)
                                ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                ;;      AND (= index mid-index)
                                )]
                             ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                             [else (begin
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;AND (< low high)
                                     (set! low (add1 mid-index))
                                     ;;INV: mid-index = (sub1 low) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;     AND (= low (add1 mid-index))
                                     (set! mid-index (quotient (+ low high) 2))
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;     AND (= low (- (* mid-index 2) high))
                                     (set! index mid-index)
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (= low (- (* mid-index 2) high))
                                     ;;     AND (= index mid-index)
                                     )])))]
        ;; mid-index = (quotient (+ low high) 2) AND (OR (= index -1) (= (vector-ref vector index) num))
        ;; => index 
                
        (begin
          (set! low 0)
          (set! high (sub1 (vector-length vector)))
          (set! mid-index (quotient (vector-length vector) 2))
          (set! index mid-index)
          (index-finder!)
          index))))

;;Sample Expressions
(define INDEX-IN-VECTOR-VAL1 (local [;;natnum
              ;;Purpose: The middle index of each interval
              (define mid-index (void))

              ;;natnum
              ;;Purpose: Low value of interval being search
              (define low (void))

              ;;natnum
              ;;Purpose: High value of interval being searched
              (define high (void))

              ;;natnum
              ;;Purpose: Index of given number
              (define index (void))
              

              ;;->natnum
              ;;Purpose: Find the index of the given number in a vector
              ;;Effect: If the number is less than the mid-index, high gets mutated to (sub1 mid-index).
              ;;        If the number is greater than the mid-index, low gets mutated to (add1 mid-index).
              ;;        If low is greater than high, mid-index gets mutated to -1.
              ;;        Otherwise, mid-index is set to (quotient (+ low high) 2).
              ;;INV: mid-index = (quotient (+ low high) 2)
              ;;Termination Argument:
              ;;The Vector Interval starts as [0...(sub1 (vector-length vector))]. During each loop the function
              ;;checks if the (vector-ref vector index) is not equal to the desired number and the index is not equal
              ;;to -1. If these are false, then the loop will terminate. Otherwise the function checks if the number
              ;;is less than or greater than the (vector-ref vector mid-index). If the number is less than
              ;;(vector-ref vector mid-index), high is changed to (sub1 mid-index), then mid-index is changed
              ;;to (quotient high 2). This means that the interval has become smaller because low has stayed the same
              ;;and high has became a smaller value than it was previously. This means that the function is making progress towards termination
              ;;due to the interval becoming smaller. If the number is greater than (vector-ref vector mid-index), low is changed to
              ;;(add1 mid-index), then mid-index is changed to (quotient (+ low high) 2). This means that the interval has
              ;;became smaller because high has stayed the same and low has became closer to high. This means that the function is
              ;;making progress towards termination due to the interval becoming smaller. Eventually the interval will become
              ;;empty or (= (vector-ref vector index) num). When the interval is empty, the loop will terminate because index will
              ;;equal -1. When the driver (= (vector-ref vector index) num) is true, the number has been found, so the loop will terminate.
              (define (index-finder!)
                (while (and (not (= index -1))
                            (not (= (vector-ref VEC1 index) 3)))
                       ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (not (= (vector-ref vector index) num))
                       (cond [(> low high)
                              ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1))
                              ;;      AND (not (= (vector-ref vector index) num))
                              ;;      AND (> low high)
                              (set! index -1)]
                             ;; INV: mid-index = (quotient (+ low high) 2) AND (> low high) AND (= index -1)
                             [(< 3 (vector-ref VEC1 mid-index))
                              ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                              ;; AND (< low high)
                              (begin
                                (set! high (sub1 mid-index))
                                ;; INV: mid-index = (add1 high) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                ;;      AND (= high (sub1 mid-index))
                                (set! mid-index (quotient high 2))
                                ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                (set! index mid-index)
                                ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                ;;      AND (= index mid-index)
                                )]
                             ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                             [else (begin
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;AND (< low high)
                                     (set! low (add1 mid-index))
                                     ;;INV: mid-index = (sub1 low) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;     AND (= low (add1 mid-index))
                                     (set! mid-index (quotient (+ low high) 2))
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;     AND (= low (- (* mid-index 2) high))
                                     (set! index mid-index)
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (= low (- (* mid-index 2) high))
                                     ;;     AND (= index mid-index)
                                     )])))]
        ;; mid-index = (quotient (+ low high) 2) AND (OR (= index -1) (= (vector-ref vector index) num))
        ;; => index 
                
        (begin
          (set! low 0)
          (set! high (sub1 (vector-length VEC1)))
          (set! mid-index (quotient (vector-length VEC1) 2))
          (set! index mid-index)
          (index-finder!)
          index)))

(define INDEX-IN-VECTOR-VAL2 (local [;;natnum
              ;;Purpose: The middle index of each interval
              (define mid-index (void))

              ;;natnum
              ;;Purpose: Low value of interval being search
              (define low (void))

              ;;natnum
              ;;Purpose: High value of interval being searched
              (define high (void))

              ;;natnum
              ;;Purpose: Index of given number
              (define index (void))
              

              ;;->natnum
              ;;Purpose: Find the index of the given number in a vector
              ;;Effect: If the number is less than the mid-index, high gets mutated to (sub1 mid-index).
              ;;        If the number is greater than the mid-index, low gets mutated to (add1 mid-index).
              ;;        If low is greater than high, mid-index gets mutated to -1.
              ;;        Otherwise, mid-index is set to (quotient (+ low high) 2).
              ;;INV: mid-index = (quotient (+ low high) 2)
              ;;Termination Argument:
              ;;The Vector Interval starts as [0...(sub1 (vector-length vector))]. During each loop the function
              ;;checks if the (vector-ref vector index) is not equal to the desired number and the index is not equal
              ;;to -1. If these are false, then the loop will terminate. Otherwise the function checks if the number
              ;;is less than or greater than the (vector-ref vector mid-index). If the number is less than
              ;;(vector-ref vector mid-index), high is changed to (sub1 mid-index), then mid-index is changed
              ;;to (quotient high 2). This means that the interval has become smaller because low has stayed the same
              ;;and high has became a smaller value than it was previously. This means that the function is making progress towards termination
              ;;due to the interval becoming smaller. If the number is greater than (vector-ref vector mid-index), low is changed to
              ;;(add1 mid-index), then mid-index is changed to (quotient (+ low high) 2). This means that the interval has
              ;;became smaller because high has stayed the same and low has became closer to high. This means that the function is
              ;;making progress towards termination due to the interval becoming smaller. Eventually the interval will become
              ;;empty or (= (vector-ref vector index) num). When the interval is empty, the loop will terminate because index will
              ;;equal -1. When the driver (= (vector-ref vector index) num) is true, the number has been found, so the loop will terminate.
              (define (index-finder!)
                (while (and (not (= index -1))
                            (not (= (vector-ref VEC2 index) 243)))
                       ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (not (= (vector-ref vector index) num))
                       (cond [(> low high)
                              ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1))
                              ;;      AND (not (= (vector-ref vector index) num))
                              ;;      AND (> low high)
                              (set! index -1)]
                             ;; INV: mid-index = (quotient (+ low high) 2) AND (> low high) AND (= index -1)
                             [(< 243 (vector-ref VEC2 mid-index))
                              ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                              ;; AND (< low high)
                              (begin
                                (set! high (sub1 mid-index))
                                ;; INV: mid-index = (add1 high) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                ;;      AND (= high (sub1 mid-index))
                                (set! mid-index (quotient high 2))
                                ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                (set! index mid-index)
                                ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                ;;      AND (= index mid-index)
                                )]
                             ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                             [else (begin
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;AND (< low high)
                                     (set! low (add1 mid-index))
                                     ;;INV: mid-index = (sub1 low) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;     AND (= low (add1 mid-index))
                                     (set! mid-index (quotient (+ low high) 2))
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;     AND (= low (- (* mid-index 2) high))
                                     (set! index mid-index)
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (= low (- (* mid-index 2) high))
                                     ;;     AND (= index mid-index)
                                     )])))]
        ;; mid-index = (quotient (+ low high) 2) AND (OR (= index -1) (= (vector-ref vector index) num))
        ;; => index 
                
        (begin
          (set! low 0)
          (set! high (sub1 (vector-length VEC2)))
          (set! mid-index (quotient (vector-length VEC2) 2))
          (set! index mid-index)
          (index-finder!)
          index)))

(define INDEX-IN-VECTOR-VAL3 (local [;;natnum
              ;;Purpose: The middle index of each interval
              (define mid-index (void))

              ;;natnum
              ;;Purpose: Low value of interval being search
              (define low (void))

              ;;natnum
              ;;Purpose: High value of interval being searched
              (define high (void))

              ;;natnum
              ;;Purpose: Index of given number
              (define index (void))
              

              ;;->natnum
              ;;Purpose: Find the index of the given number in a vector
              ;;Effect: If the number is less than the mid-index, high gets mutated to (sub1 mid-index).
              ;;        If the number is greater than the mid-index, low gets mutated to (add1 mid-index).
              ;;        If low is greater than high, mid-index gets mutated to -1.
              ;;        Otherwise, mid-index is set to (quotient (+ low high) 2).
              ;;INV: mid-index = (quotient (+ low high) 2)
              ;;Termination Argument:
              ;;The Vector Interval starts as [0...(sub1 (vector-length vector))]. During each loop the function
              ;;checks if the (vector-ref vector index) is not equal to the desired number and the index is not equal
              ;;to -1. If these are false, then the loop will terminate. Otherwise the function checks if the number
              ;;is less than or greater than the (vector-ref vector mid-index). If the number is less than
              ;;(vector-ref vector mid-index), high is changed to (sub1 mid-index), then mid-index is changed
              ;;to (quotient high 2). This means that the interval has become smaller because low has stayed the same
              ;;and high has became a smaller value than it was previously. This means that the function is making progress towards termination
              ;;due to the interval becoming smaller. If the number is greater than (vector-ref vector mid-index), low is changed to
              ;;(add1 mid-index), then mid-index is changed to (quotient (+ low high) 2). This means that the interval has
              ;;became smaller because high has stayed the same and low has became closer to high. This means that the function is
              ;;making progress towards termination due to the interval becoming smaller. Eventually the interval will become
              ;;empty or (= (vector-ref vector index) num). When the interval is empty, the loop will terminate because index will
              ;;equal -1. When the driver (= (vector-ref vector index) num) is true, the number has been found, so the loop will terminate.
              (define (index-finder!)
                (while (and (not (= index -1))
                            (not (= (vector-ref VEC3 index) -3)))
                       ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (not (= (vector-ref vector index) num))
                       (cond [(> low high)
                              ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1))
                              ;;      AND (not (= (vector-ref vector index) num))
                              ;;      AND (> low high)
                              (set! index -1)]
                             ;; INV: mid-index = (quotient (+ low high) 2) AND (> low high) AND (= index -1)
                             [(< -3 (vector-ref VEC3 mid-index))
                              ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                              ;; AND (< low high)
                              (begin
                                (set! high (sub1 mid-index))
                                ;; INV: mid-index = (add1 high) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                ;;      AND (= high (sub1 mid-index))
                                (set! mid-index (quotient high 2))
                                ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                (set! index mid-index)
                                ;; INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (> (vector-ref vector index) num))
                                ;;      AND (= index mid-index)
                                )]
                             ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                             [else (begin
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;AND (< low high)
                                     (set! low (add1 mid-index))
                                     ;;INV: mid-index = (sub1 low) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;     AND (= low (add1 mid-index))
                                     (set! mid-index (quotient (+ low high) 2))
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (< (vector-ref vector index) num))
                                     ;;     AND (= low (- (* mid-index 2) high))
                                     (set! index mid-index)
                                     ;;INV: mid-index = (quotient (+ low high) 2) AND (not (= index -1)) AND (= low (- (* mid-index 2) high))
                                     ;;     AND (= index mid-index)
                                     )])))]
        ;; mid-index = (quotient (+ low high) 2) AND (OR (= index -1) (= (vector-ref vector index) num))
        ;; => index 
                
        (begin
          (set! low 0)
          (set! high (sub1 (vector-length VEC3)))
          (set! mid-index (quotient (vector-length VEC3) 2))
          (set! index mid-index)
          (index-finder!)
          index)))

;;Tests using sample computations
(check-expect (index-in-vector 3 VEC1) INDEX-IN-VECTOR-VAL1)
(check-expect (index-in-vector 243 VEC2) INDEX-IN-VECTOR-VAL2)
(check-expect (index-in-vector -3 VEC3) INDEX-IN-VECTOR-VAL3)

;;Tests using sample values
(check-expect (index-in-vector 50 (vector 1 5 9 50 76 80)) 3)
(check-expect (index-in-vector 90 (build-vector 5000 (λ(i) i))) 90)
(check-expect (index-in-vector 501 (vector 1 9 102 306 802)) -1)
(check-expect (index-in-vector 6 (vector)) -1)

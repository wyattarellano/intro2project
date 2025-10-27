#lang htdp/isl+
;;Data Definition
;;A lon is either:
;; 1. '()
;; 2. (cons num lon)

;;Sample LON
(define LON0 '())
(define LON1 '(71 81 21 28 72 19 49 64 4 47 81 4))
(define LON2 '(91 57 93 5 16 56 61 59 93 49 -3))
(define LON3 (build-list 75000 (λ (i) (- 300000 i))))
(define LON4 (build-list 50000  (λ (i) (random 500000000))))
(define LON5 (build-list 325  (λ (i) (random 50000000))))
(define LON6 (build-list 1575 (λ (i) i)))
(define LON7 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(define LON8 (build-list 100000 (λ (i) i)))
(define LON9 (build-list 200000 (λ (i) i)))
(define LON10 (build-list 50000 (λ (i) i)))

;;A lolon is either:
;; 1. '()
;; 2. (cons lon lolon)

;;Sample LOLON
(define LOLON1 (list (list 4) (list 2 3) (list 14 5)))
(define LOLON2 (list (list 1 2 9) (list 0) (list 1 6 1 99)))
(define LOLON3 (list (list 1 4) (list 2) (list 14 1 2 89)))


;; lon --> lon
;; Purpose: Sort given lon in nondecreasing order
(define (merge-sorting a-lon)
  (local [;; lon lon --> lon
          ;; Purpose: Merge the given lons in nondecreasing order
          ;; Assumption: Given lons are in nondecreasing order
          (define (merge l1 l2)
            (cond [(empty? l1) l2]
                  [(empty? l2) l1]
                  [(<= (first l1) (first l2))
                   (cons (first l1) (merge (rest l1) l2))]
                  [else (cons (first l2) (merge l1 (rest l2)))]))
          ;; (listof lon) --> (listof lon)
          ;; Purpose: Merge every two adjacent lons in nondecreasing order
          ;; How: If the given (listof lon) has a length less
          ;;      than 2 there are no lons to merge and the
          ;;      answer is the given (listof lon). Otherwise,
          ;;      the merging of the first two lons is added
          ;;      to the front of the result of processing
          ;;      all the remaining lons after the first two.
          ;; Assumption: Nested lons are in nondecreasing order
          (define (merge-neighs a-lolon)
            (if (< (length a-lolon) 2)
                a-lolon
                (local [(define NEW-LOLON (rest (rest a-lolon)))]
                  (cons (merge (first a-lolon) (second a-lolon))
                        (merge-neighs NEW-LOLON)))))
          ;; Termination Argument:
          ;; The function halts if the given lolon has a lengths less than
          ;; 2. When given a lolon of greater length, the first two elements
          ;; are removed from it to make a recursive call. This means that the
          ;; given lolon eventually becomes empty if its length is even and
          ;; eventually becomes a list of length 1 if its length is odd. In both
          ;; cases the function terminates because the length is less than 2.
          
          ;; (listof lon) --> (listof lon)
          ;; Purpose: Sort the numbers in the given (listof lon)
          ;; How: If the length of the given (listof lon) is 1 return it.
          ;;      Otherwise, every two neighboring lons are merged to
          ;;      create a new problem instance that is recursively
          ;;      processed.
          ;; Assumption: The given (listof lon) has a length greater or equal to 1
          (define (merge-sort-helper a-lolon)
            (if (= (length a-lolon) 1)
                a-lolon
                (local [(define NEW-LOLON (merge-neighs a-lolon))]
                  (merge-sort-helper NEW-LOLON))))
          
          ;; Termination Argument:
          ;; Assume that the function merge-neighs terminates and works.
          ;; The value returned by merge-neighs is always shorter than
          ;; a-lolon because neighboring lons are merged. This means that
          ;; merge-sort-helper is always called with a shorter lolon. Observe
          ;; that this shorter lolon is never empty because merging neighbors
          ;; in a lolon of length greater than 1 never produces an empty lolon.
          ;; Given that the argument to merge-sort-helper is always shorter and
          ;; never length 0 we may conclude that eventually merge-sort-helper
          ;; is given a lolon of length 1 and the function terminates.
          ]

    
    (if (empty? a-lon)
        '()
        (first (merge-sort-helper (map (λ (n) (list n)) a-lon))))))




(define SAMP-MERGE-SORT-VAL1 (local [;; lon lon --> lon
                                     ;; Purpose: Merge the given lons in nondecreasing order
                                     ;; Assumption: Given lons are in nondecreasing order
                                     (define (merge l1 l2)
                                       (cond [(empty? l1) l2]
                                             [(empty? l2) l1]
                                             [(<= (first l1) (first l2))
                                              (cons (first l1) (merge (rest l1) l2))]
                                             [else (cons (first l2) (merge l1 (rest l2)))]))
                                     ;; (listof lon) --> (listof lon)
                                     ;; Purpose: Merge every two adjacent lons in nondecreasing order
                                     ;; How: If the given (listof lon) has a length less
                                     ;;      than 2 there are no lons to merge and the
                                     ;;      answer is the given (listof lon). Otherwise,
                                     ;;      the merging of the first two lons is added
                                     ;;      to the front of the result of processing
                                     ;;      all the remaining lons after the first two.
                                     ;; Assumption: Nested lons are in nondecreasing order
                                     (define (merge-neighs a-lolon)
                                       (if (< (length a-lolon) 2)
                                           a-lolon
                                           (local [(define NEW-LOLON (rest (rest a-lolon)))]
                                             (cons (merge (first a-lolon) (second a-lolon))
                                                   (merge-neighs NEW-LOLON)))))
                                     ;; Termination Argument:
                                     ;; The function halts if the given lolon has a lengths less than
                                     ;; 2. When given a lolon of greater length, the first two elements
                                     ;; are removed from it to make a recursive call. This means that the
                                     ;; given lolon eventually becomes empty if its length is even and
                                     ;; eventually becomes a list of length 1 if its length is odd. In both
                                     ;; cases the function terminates because the length is less than 2.
          
                                     ;; (listof lon) --> (listof lon)
                                     ;; Purpose: Sort the numbers in the given (listof lon)
                                     ;; How: If the length of the given (listof lon) is 1 return it.
                                     ;;      Otherwise, every two neighboring lons are merged to
                                     ;;      create a new problem instance that is recursively
                                     ;;      processed.
                                     ;; Assumption: The given (listof lon) has a length greater or equal to 1
                                     (define (merge-sort-helper a-lolon)
                                       (if (= (length a-lolon) 1)
                                           a-lolon
                                           (local [(define NEW-LOLON (merge-neighs a-lolon))]
                                             (merge-sort-helper NEW-LOLON))))
          
                                     ;; Termination Argument:
                                     ;; Assume that the function merge-neighs terminates and works.
                                     ;; The value returned by merge-neighs is always shorter than
                                     ;; a-lolon because neighboring lons are merged. This means that
                                     ;; merge-sort-helper is always called with a shorter lolon. Observe
                                     ;; that this shorter lolon is never empty because merging neighbors
                                     ;; in a lolon of length greater than 1 never produces an empty lolon.
                                     ;; Given that the argument to merge-sort-helper is always shorter and
                                     ;; never length 0 we may conclude that eventually merge-sort-helper
                                     ;; is given a lolon of length 1 and the function terminates.
                                     ]

    
                               (if (empty? LON1)
                                   '()
                                   (first (merge-sort-helper (map (λ (n) (list n)) LON1))))))

(define SAMP-MERGE-SORT-VAL2 (local [;; lon lon --> lon
                                     ;; Purpose: Merge the given lons in nondecreasing order
                                     ;; Assumption: Given lons are in nondecreasing order
                                     (define (merge l1 l2)
                                       (cond [(empty? l1) l2]
                                             [(empty? l2) l1]
                                             [(<= (first l1) (first l2))
                                              (cons (first l1) (merge (rest l1) l2))]
                                             [else (cons (first l2) (merge l1 (rest l2)))]))
                                     ;; (listof lon) --> (listof lon)
                                     ;; Purpose: Merge every two adjacent lons in nondecreasing order
                                     ;; How: If the given (listof lon) has a length less
                                     ;;      than 2 there are no lons to merge and the
                                     ;;      answer is the given (listof lon). Otherwise,
                                     ;;      the merging of the first two lons is added
                                     ;;      to the front of the result of processing
                                     ;;      all the remaining lons after the first two.
                                     ;; Assumption: Nested lons are in nondecreasing order
                                     (define (merge-neighs a-lolon)
                                       (if (< (length a-lolon) 2)
                                           a-lolon
                                           (local [(define NEW-LOLON (rest (rest a-lolon)))]
                                             (cons (merge (first a-lolon) (second a-lolon))
                                                   (merge-neighs NEW-LOLON)))))
                                     ;; Termination Argument:
                                     ;; The function halts if the given lolon has a lengths less than
                                     ;; 2. When given a lolon of greater length, the first two elements
                                     ;; are removed from it to make a recursive call. This means that the
                                     ;; given lolon eventually becomes empty if its length is even and
                                     ;; eventually becomes a list of length 1 if its length is odd. In both
                                     ;; cases the function terminates because the length is less than 2.
          
                                     ;; (listof lon) --> (listof lon)
                                     ;; Purpose: Sort the numbers in the given (listof lon)
                                     ;; How: If the length of the given (listof lon) is 1 return it.
                                     ;;      Otherwise, every two neighboring lons are merged to
                                     ;;      create a new problem instance that is recursively
                                     ;;      processed.
                                     ;; Assumption: The given (listof lon) has a length greater or equal to 1
                                     (define (merge-sort-helper a-lolon)
                                       (if (= (length a-lolon) 1)
                                           a-lolon
                                           (local [(define NEW-LOLON (merge-neighs a-lolon))]
                                             (merge-sort-helper NEW-LOLON))))
          
                                     ;; Termination Argument:
                                     ;; Assume that the function merge-neighs terminates and works.
                                     ;; The value returned by merge-neighs is always shorter than
                                     ;; a-lolon because neighboring lons are merged. This means that
                                     ;; merge-sort-helper is always called with a shorter lolon. Observe
                                     ;; that this shorter lolon is never empty because merging neighbors
                                     ;; in a lolon of length greater than 1 never produces an empty lolon.
                                     ;; Given that the argument to merge-sort-helper is always shorter and
                                     ;; never length 0 we may conclude that eventually merge-sort-helper
                                     ;; is given a lolon of length 1 and the function terminates.
                                     ]

    
                               (if (empty? LON2)
                                   '()
                                   (first (merge-sort-helper (map (λ (n) (list n)) LON2))))))

(define SAMP-MERGE-SORT-VAL3 (local [;; lon lon --> lon
                                     ;; Purpose: Merge the given lons in nondecreasing order
                                     ;; Assumption: Given lons are in nondecreasing order
                                     (define (merge l1 l2)
                                       (cond [(empty? l1) l2]
                                             [(empty? l2) l1]
                                             [(<= (first l1) (first l2))
                                              (cons (first l1) (merge (rest l1) l2))]
                                             [else (cons (first l2) (merge l1 (rest l2)))]))
                                     ;; (listof lon) --> (listof lon)
                                     ;; Purpose: Merge every two adjacent lons in nondecreasing order
                                     ;; How: If the given (listof lon) has a length less
                                     ;;      than 2 there are no lons to merge and the
                                     ;;      answer is the given (listof lon). Otherwise,
                                     ;;      the merging of the first two lons is added
                                     ;;      to the front of the result of processing
                                     ;;      all the remaining lons after the first two.
                                     ;; Assumption: Nested lons are in nondecreasing order
                                     (define (merge-neighs a-lolon)
                                       (if (< (length a-lolon) 2)
                                           a-lolon
                                           (local [(define NEW-LOLON (rest (rest a-lolon)))]
                                             (cons (merge (first a-lolon) (second a-lolon))
                                                   (merge-neighs NEW-LOLON)))))
                                     ;; Termination Argument:
                                     ;; The function halts if the given lolon has a lengths less than
                                     ;; 2. When given a lolon of greater length, the first two elements
                                     ;; are removed from it to make a recursive call. This means that the
                                     ;; given lolon eventually becomes empty if its length is even and
                                     ;; eventually becomes a list of length 1 if its length is odd. In both
                                     ;; cases the function terminates because the length is less than 2.
          
                                     ;; (listof lon) --> (listof lon)
                                     ;; Purpose: Sort the numbers in the given (listof lon)
                                     ;; How: If the length of the given (listof lon) is 1 return it.
                                     ;;      Otherwise, every two neighboring lons are merged to
                                     ;;      create a new problem instance that is recursively
                                     ;;      processed.
                                     ;; Assumption: The given (listof lon) has a length greater or equal to 1
                                     (define (merge-sort-helper a-lolon)
                                       (if (= (length a-lolon) 1)
                                           a-lolon
                                           (local [(define NEW-LOLON (merge-neighs a-lolon))]
                                             (merge-sort-helper NEW-LOLON))))
          
                                     ;; Termination Argument:
                                     ;; Assume that the function merge-neighs terminates and works.
                                     ;; The value returned by merge-neighs is always shorter than
                                     ;; a-lolon because neighboring lons are merged. This means that
                                     ;; merge-sort-helper is always called with a shorter lolon. Observe
                                     ;; that this shorter lolon is never empty because merging neighbors
                                     ;; in a lolon of length greater than 1 never produces an empty lolon.
                                     ;; Given that the argument to merge-sort-helper is always shorter and
                                     ;; never length 0 we may conclude that eventually merge-sort-helper
                                     ;; is given a lolon of length 1 and the function terminates.
                                     ]

    
                               (if (empty? LON3)
                                   '()
                                   (first (merge-sort-helper (map (λ (n) (list n)) LON3))))))

;;Tests using sample computations
(check-expect (merge-sorting LON1) SAMP-MERGE-SORT-VAL1)
(check-expect (merge-sorting LON2) SAMP-MERGE-SORT-VAL2)
(check-expect (merge-sorting LON3) SAMP-MERGE-SORT-VAL3)

;;Tests using sample values
(check-expect (merge-sorting (list 1 7 3 0 7 9)) (list 0 1 3 7 7 9))
(check-expect (merge-sorting (list 7 19 2 0 15 7)) (list 0 2 7 7 15 19))
(check-expect (merge-sorting (list 3 89 1 50 4 7)) (list 1 3 4 7 50 89))

;; lon --> lon
;; Purpose: Sort given lon in nondecreasing order
(define (new-merge-sorting a-lon)
  (local [;; lon lon --> lon
          ;; Purpose: Merge the given lons in nondecreasing order
          ;; Assumption: Given lons are in nondecreasing order
          (define (merge l1 l2)
            (cond [(empty? l1) l2]
                  [(empty? l2) l1]
                  [(<= (first l1) (first l2))
                   (cons (first l1) (merge (rest l1) l2))]
                  [else (cons (first l2) (merge l1 (rest l2)))]))
          ;; (listof lon) --> (listof lon)
          ;; Purpose: Merge every two adjacent lons in nondecreasing order
          ;; How: If the given (listof lon) has a length less
          ;;      than 2 then the given lolon is appended to
          ;;      the accumulator. Otherwise, the function is
          ;;      recursively called on the rest of the rest of the
          ;;      list with merged first two elements being added
          ;;      to the front of the accumulator.
          ;; Accumulator Invariant: accum = List of elements processed of
          ;;                        the given lolon with the most recently
          ;;                        processed element being the first of the list.
          ;; Assumption: Nested lons are in nondecreasing order
          (define (merge-neighs a-lolon accum)
            (if (< (length a-lolon) 2)
                (append a-lolon accum)
                (local [(define ACCUM (merge (first a-lolon) (second a-lolon)))
                        (define NEW-LOLON (rest (rest a-lolon)))]
                  (merge-neighs NEW-LOLON (cons ACCUM accum)))))
          ;; Termination Argument:
          ;; The function halts if the given lolon has a lengths less than
          ;; 2. When given a lolon of greater length, the first two elements
          ;; are removed from it to make a recursive call. This means that the
          ;; given lolon eventually becomes empty if its length is even and
          ;; eventually becomes a list of length 1 if its length is odd. In both
          ;; cases the function terminates because the length is less than 2.
          
          ;; (listof lon) --> (listof lon)
          ;; Purpose: Sort the numbers in the given (listof lon)
          ;; How: If the length of the given (listof lon) is 1 return it.
          ;;      Otherwise, every two neighboring lons are merged to
          ;;      create a new problem instance that is recursively
          ;;      processed.
          ;; Assumption: The given (listof lon) has a length greater or equal to 1
          (define (merge-sort-helper a-lolon)
            (if (= (length a-lolon) 1)
                a-lolon
                (local [(define NEW-LOLON (merge-neighs a-lolon '()))]
                  (merge-sort-helper NEW-LOLON))))
          
          ;; Termination Argument:
          ;; Assume that the function merge-neighs terminates and works.
          ;; The value returned by merge-neighs is always shorter than
          ;; a-lolon because neighboring lons are merged. This means that
          ;; merge-sort-helper is always called with a shorter lolon. Observe
          ;; that this shorter lolon is never empty because merging neighbors
          ;; in a lolon of length greater than 1 never produces an empty lolon.
          ;; Given that the argument to merge-sort-helper is always shorter and
          ;; never length 0 we may conclude that eventually merge-sort-helper
          ;; is given a lolon of length 1 and the function terminates.
          ]

    
    (if (empty? a-lon)
        '()
        (first (merge-sort-helper (map (λ (n) (list n)) a-lon))))))


(define SAMP-NEW-MERGE-SORT-VAL1 (local [;; lon lon --> lon
                                         ;; Purpose: Merge the given lons in nondecreasing order
                                         ;; Assumption: Given lons are in nondecreasing order
                                         (define (merge l1 l2)
                                           (cond [(empty? l1) l2]
                                                 [(empty? l2) l1]
                                                 [(<= (first l1) (first l2))
                                                  (cons (first l1) (merge (rest l1) l2))]
                                                 [else (cons (first l2) (merge l1 (rest l2)))]))
                                         ;; (listof lon) --> (listof lon)
                                         ;; Purpose: Merge every two adjacent lons in nondecreasing order
                                         ;; How: If the given (listof lon) has a length less
                                         ;;      than 2 then the given lolon is appended to
                                         ;;      the accumulator. Otherwise, the function is
                                         ;;      recursively called on the rest of the rest of the
                                         ;;      list with merged first two elements being added
                                         ;;      to the front of the accumulator.
                                         ;; Accumulator Invariant: accum = List of elements processed of
                                         ;;                        the given lolon with the most recently
                                         ;;                        processed element being the first of the list.
                                         ;; Assumption: Nested lons are in nondecreasing order
                                         (define (merge-neighs a-lolon accum)
                                           (if (< (length a-lolon) 2)
                                               (append a-lolon accum)
                                               (local [(define ACCUM (merge (first a-lolon) (second a-lolon)))
                                                       (define NEW-LOLON (rest (rest a-lolon)))]
                                                 (merge-neighs NEW-LOLON (cons ACCUM accum)))))
                                         ;; Termination Argument:
                                         ;; The function halts if the given lolon has a lengths less than
                                         ;; 2. When given a lolon of greater length, the first two elements
                                         ;; are removed from it to make a recursive call. This means that the
                                         ;; given lolon eventually becomes empty if its length is even and
                                         ;; eventually becomes a list of length 1 if its length is odd. In both
                                         ;; cases the function terminates because the length is less than 2.
          
                                         ;; (listof lon) --> (listof lon)
                                         ;; Purpose: Sort the numbers in the given (listof lon)
                                         ;; How: If the length of the given (listof lon) is 1 return it.
                                         ;;      Otherwise, every two neighboring lons are merged to
                                         ;;      create a new problem instance that is recursively
                                         ;;      processed.
                                         ;; Assumption: The given (listof lon) has a length greater or equal to 1
                                         (define (merge-sort-helper a-lolon)
                                           (if (= (length a-lolon) 1)
                                               a-lolon
                                               (local [(define NEW-LOLON (merge-neighs a-lolon '()))]
                                                 (merge-sort-helper NEW-LOLON))))
          
                                         ;; Termination Argument:
                                         ;; Assume that the function merge-neighs terminates and works.
                                         ;; The value returned by merge-neighs is always shorter than
                                         ;; a-lolon because neighboring lons are merged. This means that
                                         ;; merge-sort-helper is always called with a shorter lolon. Observe
                                         ;; that this shorter lolon is never empty because merging neighbors
                                         ;; in a lolon of length greater than 1 never produces an empty lolon.
                                         ;; Given that the argument to merge-sort-helper is always shorter and
                                         ;; never length 0 we may conclude that eventually merge-sort-helper
                                         ;; is given a lolon of length 1 and the function terminates.
                                         ]

    
                                   (if (empty? LON1)
                                       '()
                                       (first (merge-sort-helper (map (λ (n) (list n)) LON1))))))

(define SAMP-NEW-MERGE-SORT-VAL2 (local [;; lon lon --> lon
                                         ;; Purpose: Merge the given lons in nondecreasing order
                                         ;; Assumption: Given lons are in nondecreasing order
                                         (define (merge l1 l2)
                                           (cond [(empty? l1) l2]
                                                 [(empty? l2) l1]
                                                 [(<= (first l1) (first l2))
                                                  (cons (first l1) (merge (rest l1) l2))]
                                                 [else (cons (first l2) (merge l1 (rest l2)))]))
                                         ;; (listof lon) --> (listof lon)
                                         ;; Purpose: Merge every two adjacent lons in nondecreasing order
                                         ;; How: If the given (listof lon) has a length less
                                         ;;      than 2 then the given lolon is appended to
                                         ;;      the accumulator. Otherwise, the function is
                                         ;;      recursively called on the rest of the rest of the
                                         ;;      list with merged first two elements being added
                                         ;;      to the front of the accumulator.
                                         ;; Accumulator Invariant: accum = List of elements processed of
                                         ;;                        the given lolon with the most recently
                                         ;;                        processed element being the first of the list.
                                         ;; Assumption: Nested lons are in nondecreasing order
                                         (define (merge-neighs a-lolon accum)
                                           (if (< (length a-lolon) 2)
                                               (append a-lolon accum)
                                               (local [(define ACCUM (merge (first a-lolon) (second a-lolon)))
                                                       (define NEW-LOLON (rest (rest a-lolon)))]
                                                 (merge-neighs NEW-LOLON (cons ACCUM accum)))))
                                         ;; Termination Argument:
                                         ;; The function halts if the given lolon has a lengths less than
                                         ;; 2. When given a lolon of greater length, the first two elements
                                         ;; are removed from it to make a recursive call. This means that the
                                         ;; given lolon eventually becomes empty if its length is even and
                                         ;; eventually becomes a list of length 1 if its length is odd. In both
                                         ;; cases the function terminates because the length is less than 2.
          
                                         ;; (listof lon) --> (listof lon)
                                         ;; Purpose: Sort the numbers in the given (listof lon)
                                         ;; How: If the length of the given (listof lon) is 1 return it.
                                         ;;      Otherwise, every two neighboring lons are merged to
                                         ;;      create a new problem instance that is recursively
                                         ;;      processed.
                                         ;; Assumption: The given (listof lon) has a length greater or equal to 1
                                         (define (merge-sort-helper a-lolon)
                                           (if (= (length a-lolon) 1)
                                               a-lolon
                                               (local [(define NEW-LOLON (merge-neighs a-lolon '()))]
                                                 (merge-sort-helper NEW-LOLON))))
          
                                         ;; Termination Argument:
                                         ;; Assume that the function merge-neighs terminates and works.
                                         ;; The value returned by merge-neighs is always shorter than
                                         ;; a-lolon because neighboring lons are merged. This means that
                                         ;; merge-sort-helper is always called with a shorter lolon. Observe
                                         ;; that this shorter lolon is never empty because merging neighbors
                                         ;; in a lolon of length greater than 1 never produces an empty lolon.
                                         ;; Given that the argument to merge-sort-helper is always shorter and
                                         ;; never length 0 we may conclude that eventually merge-sort-helper
                                         ;; is given a lolon of length 1 and the function terminates.
                                         ]

    
                                   (if (empty? LON2)
                                       '()
                                       (first (merge-sort-helper (map (λ (n) (list n)) LON2))))))

(define SAMP-NEW-MERGE-SORT-VAL3 (local [;; lon lon --> lon
                                         ;; Purpose: Merge the given lons in nondecreasing order
                                         ;; Assumption: Given lons are in nondecreasing order
                                         (define (merge l1 l2)
                                           (cond [(empty? l1) l2]
                                                 [(empty? l2) l1]
                                                 [(<= (first l1) (first l2))
                                                  (cons (first l1) (merge (rest l1) l2))]
                                                 [else (cons (first l2) (merge l1 (rest l2)))]))
                                         ;; (listof lon) --> (listof lon)
                                         ;; Purpose: Merge every two adjacent lons in nondecreasing order
                                         ;; How: If the given (listof lon) has a length less
                                         ;;      than 2 then the given lolon is appended to
                                         ;;      the accumulator. Otherwise, the merged first two elements
                                         ;;      are added to the front of the accumulator. Then, the function is
                                         ;;      recursively called on the rest of the rest of the
                                         ;;      list with the updated accumulator.
                                         ;; Accumulator Invariant: accum = List of elements processed of
                                         ;;                        the given lolon with the most recently
                                         ;;                        processed element being the first of the list.
                                         ;; Assumption: Nested lons are in nondecreasing order
                                         (define (merge-neighs a-lolon accum)
                                           (if (< (length a-lolon) 2)
                                               (append a-lolon accum)
                                               (local [(define ACCUM (merge (first a-lolon) (second a-lolon)))
                                                       (define NEW-LOLON (rest (rest a-lolon)))]
                                                 (merge-neighs NEW-LOLON (cons ACCUM accum)))))
                                         ;; Termination Argument:
                                         ;; The function halts if the given lolon has a lengths less than
                                         ;; 2. When given a lolon of greater length, the first two elements
                                         ;; are removed from it to make a recursive call. This means that the
                                         ;; given lolon eventually becomes empty if its length is even and
                                         ;; eventually becomes a list of length 1 if its length is odd. In both
                                         ;; cases the function terminates because the length is less than 2.
          
                                         ;; (listof lon) --> (listof lon)
                                         ;; Purpose: Sort the numbers in the given (listof lon)
                                         ;; How: If the length of the given (listof lon) is 1 return it.
                                         ;;      Otherwise, every two neighboring lons are merged to
                                         ;;      create a new problem instance that is recursively
                                         ;;      processed.
                                         ;; Assumption: The given (listof lon) has a length greater or equal to 1
                                         (define (merge-sort-helper a-lolon)
                                           (if (= (length a-lolon) 1)
                                               a-lolon
                                               (local [(define NEW-LOLON (merge-neighs a-lolon '()))]
                                                 (merge-sort-helper NEW-LOLON))))
          
                                         ;; Termination Argument:
                                         ;; Assume that the function merge-neighs terminates and works.
                                         ;; The value returned by merge-neighs is always shorter than
                                         ;; a-lolon because neighboring lons are merged. This means that
                                         ;; merge-sort-helper is always called with a shorter lolon. Observe
                                         ;; that this shorter lolon is never empty because merging neighbors
                                         ;; in a lolon of length greater than 1 never produces an empty lolon.
                                         ;; Given that the argument to merge-sort-helper is always shorter and
                                         ;; never length 0 we may conclude that eventually merge-sort-helper
                                         ;; is given a lolon of length 1 and the function terminates.
                                         ]

    
                                   (if (empty? LON3)
                                       '()
                                       (first (merge-sort-helper (map (λ (n) (list n)) LON3))))))

;;Tests using sample computations
(check-expect (new-merge-sorting LON1) SAMP-NEW-MERGE-SORT-VAL1)
(check-expect (new-merge-sorting LON2) SAMP-NEW-MERGE-SORT-VAL2)
(check-expect (new-merge-sorting LON3) SAMP-NEW-MERGE-SORT-VAL3)

;;Tests using sample values
(check-expect (new-merge-sorting (list 1 7 3 0 7 9)) (list 0 1 3 7 7 9))
(check-expect (new-merge-sorting (list 7 19 2 0 15 7)) (list 0 2 7 7 15 19))
(check-expect (new-merge-sorting (list 3 89 1 50 4 7)) (list 1 3 4 7 50 89))


;;Timing the functions
"New Merge"
(define T1-NM (time (new-merge-sorting LON1)))
(define T2-NM (time (new-merge-sorting LON2)))
(define T3-NM (time (new-merge-sorting LON3)))
(define T4-NM (time (new-merge-sorting LON4)))
(define T5-NM (time (new-merge-sorting LON5)))
(define T6-NM (time (new-merge-sorting LON6)))
(define T7-NM (time (new-merge-sorting LON7)))
(define T8-NM (time (new-merge-sorting LON8)))
(define T9-NM (time (new-merge-sorting LON9)))
(define T10-NM (time (new-merge-sorting LON10)))
"Old Merge"
(define T1-OLD-MS1 (time (merge-sorting LON1)))
(define T1-OLD-MS2 (time (merge-sorting LON2)))
(define T1-OLD-MS3 (time (merge-sorting LON3)))
(define T1-OLD-MS4 (time (merge-sorting LON4)))
(define T1-OLD-MS5 (time (merge-sorting LON5)))
(define T1-OLD-MS6 (time (merge-sorting LON6)))
(define T1-OLD-MS7 (time (merge-sorting LON7)))
(define T1-OLD-MS8 (time (merge-sorting LON8)))
(define T1-OLD-MS9 (time (merge-sorting LON9)))
(define T1-OLD-MS10 (time (merge-sorting LON10)))

#|
"New Merge"
cpu time: 0 real time: 0 gc time: 0
cpu time: 0 real time: 0 gc time: 0
cpu time: 1640 real time: 5762 gc time: 0
cpu time: 765 real time: 2614 gc time: 0
cpu time: 0 real time: 1 gc time: 0
cpu time: 0 real time: 9 gc time: 0
cpu time: 0 real time: 0 gc time: 0
cpu time: 2484 real time: 9467 gc time: 0
cpu time: 15625 real time: 45208 gc time: 93
cpu time: 687 real time: 2591 gc time: 0
"Old Merge"
cpu time: 0 real time: 0 gc time: 0
cpu time: 0 real time: 0 gc time: 0
cpu time: 1265 real time: 4821 gc time: 15
cpu time: 687 real time: 2447 gc time: 46
cpu time: 0 real time: 1 gc time: 0
cpu time: 0 real time: 10 gc time: 0
cpu time: 0 real time: 0 gc time: 0
cpu time: 2187 real time: 8421 gc time: 0
cpu time: 12531 real time: 39886 gc time: 31
cpu time: 546 real time: 2175 gc time: 0

The new merge-sort with an accumulator is slower than the original merge-sort.
The only function that was changed was merge-neighs. The complexity of the original
merge-neighs function is 2n + 2k, where 2k is the length of the sublists of the
given lolon. The complexity of the new merge-neighs function is 3n + 2k, where
2k is the length of the sublists of the given lolon. This is because in the new
merge-neighs there is an append, which traveses the given lolon. Therefore an
additional n operation occurs, which in turn increases the complexity of the
function. This increase in complexity results in the new merge-sort taking a
longer time to run compared to the orginial merge-sort. This can be see in our
time tests comparing the run time of both functions.
|#
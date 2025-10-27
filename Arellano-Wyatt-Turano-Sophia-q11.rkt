#lang htdp/asl
(require while)

;;Nat Nat -> Nat
;;Purpose: To add two numbers using repetitive addition
;;Effect: Curr-a is decremented by 1, and
;;        sum is increased by 1.
(define (plus a b)
  (cond [(or (< a 0)
             (< b 0))
         (error "Both numbers must be natural numbers")]
        [(= a 0) b]
        [else (local [;;natnum
                      ;;Purpose: Natural number to be added to b
                      (define curr-a (void))

                      ;;natnum
                      ;;Purpose: Current sum
                      (define accum (void))]
                ;; INV: accum = (+ b (- a curr-a)) AND (< 0 a)
                (begin
                  ;; INV: accum = (+ b (- a curr-a)) AND (< 0 a)
                  (set! curr-a a)
                  ;; INV: accum = (+ b (- a curr-a)) AND (< 0 a) AND (< 0 curr-a)
                  (set! accum b)
                  ;; INV: accum = (+ b (- a curr-a)) AND (< 0 a) AND (< 0 curr-a)
                  (while  (< 0 curr-a)
                         ;; INV: accum = (+ b (- a curr-a)) AND  (< 0 curr-a)
                         (set! curr-a (sub1 curr-a))
                         ;; INV: accum = (+ b (- a (add1 curr-a))) AND (>= curr-a 0)
                         (set! accum (add1 accum))
                         ;; INV: accum = (+ b (- a curr-a)) AND (>= curr-a 0)
                         )
                  ;;accum = (+ b (- a curr-a)) AND (= curr-a 0)
                  ;;=> accum = (+ b a)
                  accum)
                ;; Termination Argument:
                ;; curr-a starts at a natnum > 0. Each loop iteration
                ;; decrements curr-a by 1. Eventually, curr-a becomes
                ;; 0 and the loop terminates.
                )]))

;;Sample Expressions
(define PLUS-VAL1 15)
(define PLUS-VAL2 (local [;;natnum
                      ;;Purpose: Natural number to be added to b
                      (define curr-a (void))

                      ;;natnum
                      ;;Purpose: Current sum
                      (define accum (void))]
                 ;; INV: accum = (+ b (- a curr-a)) AND (< 0 a)
                (begin
                  ;; INV: accum = (+ b (- a curr-a)) AND (< 0 a)
                  (set! curr-a 20)
                  ;; INV: accum = (+ b (- a curr-a)) AND (< 0 a) AND (< 0 curr-a)
                  (set! accum 25)
                  ;; INV: accum = (+ b (- a curr-a)) AND (< 0 a) AND (< 0 curr-a)
                  (while  (< 0 curr-a)
                         ;; INV: accum = (+ b (- a curr-a)) AND  (< 0 curr-a)
                         (set! curr-a (sub1 curr-a))
                         ;; INV: accum = (+ b (- a (add1 curr-a))) AND (>= curr-a 0)
                         (set! accum (add1 accum))
                         ;; INV: accum = (+ b (- a curr-a)) AND (>= curr-a 0)
                         )
                  ;;accum = (+ b (- a curr-a)) AND (= curr-a 0)
                  ;;=> accum = (+ b a)
                  accum)
                ;; Termination Argument:
                ;; curr-a starts at a natnum > 0. Each loop iteration
                ;; decrements curr-a by 1. Eventually, curr-a becomes
                ;; 0 and the loop terminates.
                ))

;;Tests using sample computations
(check-expect (plus 0 15) PLUS-VAL1)
(check-expect (plus 20 25) PLUS-VAL2)

;;Tests for plus
(check-error (plus -8 10) "Both numbers must be natural numbers")
(check-error (plus 8 -10) "Both numbers must be natural numbers")
(check-expect (plus 19 3) 22)
(check-expect (plus 4 14) 18)
(check-expect (plus 0 10) 10)
(check-expect (plus 8 0) 8)
(check-expect (plus 0 0) 0)

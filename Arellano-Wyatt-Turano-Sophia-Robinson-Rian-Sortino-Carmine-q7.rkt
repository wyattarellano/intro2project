#lang htdp/isl+

;; A listof number is (cons number lon)

;; Sample instances of a lon
(define LON1 '(1 2 3 4))
(define LON2 '(3 0 9 1))
(define LON3 '(990999 29 1 10000))


;; lon -> number
;; Purpose: To find the maximum of a non-empty lon 
;; Assumption: The given lon is not empty
;; Accumulator Invariant: The max of the processed elements in the given lon
(define (maxx a-lon)
  (inexact->exact (foldl max -inf.0 a-lon)))

;; Sample expressions for maxx
(define LON1-VAL
  (inexact->exact (foldl max -inf.0 LON1)))
(define LON2-VAL
  (inexact->exact (foldl max -inf.0 LON2)))
(define LON3-VAL
  (inexact->exact (foldl max -inf.0 LON3)))

;; Tests using sample expressions for maxx
(check-expect (maxx LON1) LON1-VAL)
(check-expect (maxx LON2) LON2-VAL)
(check-expect (maxx LON3) LON3-VAL)

;; Tests using sample values for maxx
(check-expect (maxx '(12 13 24 56 43)) 56)
(check-expect (maxx '(34 76 235 6 0)) 235)
(check-expect (maxx '(1 4 3 -6 0)) 4)
(check-expect (maxx '(-1 -10 -9 -2 -100)) -1)

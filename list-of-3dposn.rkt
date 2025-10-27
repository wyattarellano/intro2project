#lang htdp/isl+
;; Design and implement a program that converts a given list if three-dimensional coordinates represented using a string of numbers to a
;; list of three-dimentional coordinates represented using structures

;; A 3dposn is a structure: (make-3dposn x y z)
(define-struct 3dposn (x y z))

;; sample instances of 3dposn
(define 3DPOSN1 (make-3dposn 1 2 3))
(define 3DPOSN2 (make-3dposn 2 4 6))

;; A list of 3dposn (lo3d) is either:
;; 1. '()
;; 2. (cons 3dposn lo3d)

;; sample instances of lo3d
(define LO3D1 '(3DPOSN1 3DPOSN2))
(define LO3D2 '((make-3dposn (5 6 7)) (make-3dposn (1 9 4))))

;; A list of number (lon) is either:
;;  1. '()
;;  2. (cons number lon)

;; sample instances of lon
(define LON1 '(1 2 3 4 5 6 7 8 9))
(define LON2 '(2 3 4 4 5 6 7 6 4 3))
(define LON3 '(2 4 5 5 2 5 6 7 5 5 2))


;; lon -> lo3d error
;; Purpose: To convert a given list of numbers to a list of 3dposn coordinates represented using structures
;; How: If the remainder of the given lon is less than 3, then it returns an error. The function terminates when
;;      the list is empty. Otherwise a 3dposn is created using the first, second, and third of the given lon.
;;      Then the function cons the 3dposn to the recursively processed rest of the lon, thus making the lon shorter
;;      after each recursive call. This will result in the list becoming empty.
(define (lon->lo3d a-lon)
  (local [;; lon -> 3dposn
          ;; Purpose: To make a 3dposn using the given lon
          (define (mk-3dposn a-lon)
            (make-3dposn (first a-lon) (second a-lon) (third a-lon)))

          ;; lon -> lon
          ;; Purpose: Make a new lon from the 4th number on from a given lon
          (define (rest-for-lo3d a-lon)
            (rest (rest (rest a-lon))))]
    
    (cond [(> (remainder (length a-lon) 3) 0)
           (error "There is " (remainder (length a-lon) 3) " number(s) left that cannot be made into a posn")]
          [(empty? a-lon)
           '()]
          [else (cons (mk-3dposn a-lon) (lon->lo3d (rest-for-lo3d a-lon)))])))

;; sample expressions for lon->lo3d
(define SAMP-LON->LO3D1 (local [;; lon -> 3dposn
                                ;; Purpose: To make a 3dposn using the given lon
                                (define (mk-3dposn a-lon)
                                  (make-3dposn (first a-lon) (second a-lon) (third a-lon)))

                                ;; lon -> lon
                                ;; Purpose: Make a new lon from the 4th number on from a given lon
                                (define (rest-for-lo3d a-lon)
                                  (rest (rest (rest a-lon))))]
                          
                          (cond [(> (remainder (length LON1) 3) 0)
                                 (error "There is " (remainder (length LON1) 3) " number(s) left that cannot be made into a posn")]
                                [(empty? LON1)
                                 '()]
                                [else (cons (mk-3dposn LON1) (lon->lo3d (rest-for-lo3d LON1)))])))



;;tests using sample computations
(check-expect (lon->lo3d LON1) SAMP-LON->LO3D1)
(check-error (lon->lo3d LON2) "There is 1 number(s) left that cannot be made into a posn")
(check-error (lon->lo3d LON3) "There is 2 number(s) left that cannot be made into a posn")

;;tests using sample values
(check-expect (lon->lo3d (list 1 5 2 0 8 8 1 4 8)) (list (make-3dposn 1 5 2) (make-3dposn 0 8 8) (make-3dposn 1 4 8)))
(check-expect (lon->lo3d (list 2 5 32 59 2 8)) (list (make-3dposn 2 5 32) (make-3dposn 59 2 8)))
(check-error (lon->lo3d (list 2 5 32 59 2 9 10 7 90 2 7 2 8 2)) "There is 2 number(s) left that cannot be made into a posn")





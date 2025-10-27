#lang htdp/isl+

;;lon -> lon
;;Purpose: Make a lon that is sorted in nondecreasing order from a given lon 
(define (make-sublist a-lon)
  (cond [(empty? a-lon) '()]
        [(or (= (length a-lon) 1)
             (> (first a-lon) (second a-lon)))
         (list (first a-lon))]
        [else (cons (first a-lon) (make-sublist (rest a-lon)))]))

;;lon num -> lon
;;Purpose: To create a new lon from the given lon where the first n numbers in the list are dropped
(define (drop-sublist a-lon num)
  (cond [(empty? a-lon) '()]
        [(= num 0) a-lon]
        [else (drop-sublist (rest a-lon) (sub1 num))]))

;;lon -> lolon
;;Purpose: Create a lolon where each element is the list is sorted
;; How: If the given list is empty, then the function will return an empty list.
;;      Otherwise the function will cons the sublist of the given list to the
;;      recursive call of the given list without the first n elements, where n
;;      is the length of the sublist of the given lon.
(define (create-lolon a-lon)
  (if (empty? a-lon)
      '()
      (local [(define SUBLIST (make-sublist a-lon))]
        (cons SUBLIST (create-lolon (drop-sublist a-lon (length SUBLIST)))))))

;;Termination Argument: With every recursive call, the list will be shortened by drop-sublist, where
;;                      the amount of elements dropped is equal to the length of the sublist.
;;                      Thus, the list will eventually become empty and terminate the function.
;; _______________________________________________________________________________

(define (list-maker a-lon a-lon2)
  (cond 
    [(and (list? (first a-lon))
          (not (empty? a-lon2)))
     (append a-lon (list (reverse a-lon2)))]
    [(empty? a-lon) (list '())]
    [(empty? a-lon2) (list-maker (rest a-lon) (cons (first a-lon) a-lon2))]
    [(>= (first a-lon) (first a-lon2)) (list-maker (rest a-lon) (cons (first a-lon) a-lon2))]
    [else (list-maker (append a-lon (list (reverse a-lon2))) '())]))
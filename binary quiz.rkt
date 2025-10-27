#lang htdp/isl+
;; A node is a structure, (make-node X (btof X) (btof X)), with an X value and two 
;; subtrees of X values.
(define-struct node (val ltree rtree))

;; sample instances of node
(define NODE1 (make-node 4 '() '()))
(define NODE2 (make-node 5 (make-node 3 '() '()) '()))
(define NODE3 (make-node 3 (make-node 2 '() '()) '()))


;; A binary tree (bt) is either:
;;  1. '()
;;  2. node

(define SUBTREE1 (make-node 50 (make-node 25 (make-node 15 (make-node 10 '() '()) (make-node 17 '() '()))
                                          (make-node 35 (make-node 30 '() '()) (make-node 38 '() '())))
                            (make-node 75 (make-node 65 (make-node 55 '() '()) (make-node 70 '() '()))
                                       (make-node 90 (make-node 81 '() '()) (make-node 99 '() '())))))
(define SUBTREE2 (make-node 79 (make-node 48 (make-node 10 '() '()) (make-node 62 '() '()))
                                          (make-node 140 (make-node 92 '() '()) (make-node 793 '() '()))))
(define SUBTREE3 NODE1)
(define SUBTREE4 (make-node 100 (make-node 50 (make-node 15 (make-node 11 '() '()) (make-node 17 '() '()))
                                          (make-node 60 (make-node 52 '() '()) (make-node 79 '() '())))
                            (make-node 150 (make-node 125 (make-node 110 '() '()) (make-node 145 '() '()))
                                       (make-node 175 (make-node 169 '() '()) (make-node 193 '() '())))))
(define SUBTREE5 NODE3)



;; A direction (dir) is either:
;;  1. 'left
;;  2. 'right


;; A list of dir (lod) is either:
;;  1. '()
;;  2. (cons dir lod)

;; A path is a lod 


;; number bt --> string lod throws error
;; Purpose: To find the path of 'lefts and 'rights followed to reach the given number
(define (find-path a-num a-bt)
  (if (= a-num (node-val a-bt))
      "The given number is the root of the binary tree."
      (local [;; number bt lod --> lod throws error
              ;; Purpose: To find the path of 'lefts and 'rights followed to reach the given number
              ;; Accumulator Invariant: path = the directions taken through the processed nodes of a binary tree to reach a given number
              (define (find-path-helper a-num a-bt path)
                (cond [(empty? a-bt) (error "number not found in tree")]
                      [(= a-num (node-val a-bt)) path]
                      [(< a-num (node-val a-bt))
                       (find-path-helper a-num (node-ltree a-bt) (cons 'left path))]
                      [(> a-num (node-val a-bt))
                       (find-path-helper a-num (node-rtree a-bt) (cons 'right path))]
                      [else '()]))]
        (find-path-helper a-num a-bt '()))))


;; Sample Expressions
(define SAMP-PATH1 (if (= 30 (node-val SUBTREE1))
      "The given number is the root of the binary tree."
      (local [;; number bt lod --> lod throws error
              ;; Purpose: To find the path of 'lefts and 'rights followed to reach the given number
              ;; Accumulator Invariant: path = the directions taken through the processed nodes of a binary tree to reach a given number
              (define (find-path-helper a-num a-bt path)
                (cond [(empty? a-bt) (error "number not found in tree")]
                      [(= a-num (node-val a-bt)) path]
                      [(< a-num (node-val a-bt))
                       (find-path-helper a-num (node-ltree a-bt) (cons 'left path))]
                      [(> a-num (node-val a-bt))
                       (find-path-helper a-num (node-rtree a-bt) (cons 'right path))]
                      [else '()]))]
        (find-path-helper 30 SUBTREE1 '()))))


(define SAMP-PATH2 (if (= 793 (node-val SUBTREE2))
      "The given number is the root of the binary tree."
      (local [;; number bt lod --> lod throws error
              ;; Purpose: To find the path of 'lefts and 'rights followed to reach the given number
              ;; Accumulator Invariant: path = the directions taken through the processed nodes of a binary tree to reach a given number
              (define (find-path-helper a-num a-bt path)
                (cond [(empty? a-bt) (error "number not found in tree")]
                      [(= a-num (node-val a-bt)) path]
                      [(< a-num (node-val a-bt))
                       (find-path-helper a-num (node-ltree a-bt) (cons 'left path))]
                      [(> a-num (node-val a-bt))
                       (find-path-helper a-num (node-rtree a-bt) (cons 'right path))]
                      [else '()]))]
        (find-path-helper 793 SUBTREE2 '()))))

(define SAMP-PATH3 (if (= 4 (node-val SUBTREE3))
      "The given number is the root of the binary tree."
      (local [;; number bt lod --> lod throws error
              ;; Purpose: To find the path of 'lefts and 'rights followed to reach the given number
              ;; Accumulator Invariant: path = the directions taken through the processed nodes of a binary tree to reach a given number
              (define (find-path-helper a-num a-bt path)
                (cond [(empty? a-bt) (error "number not found in tree")]
                      [(= a-num (node-val a-bt)) path]
                      [(< a-num (node-val a-bt))
                       (find-path-helper a-num (node-ltree a-bt) (cons 'left path))]
                      [(> a-num (node-val a-bt))
                       (find-path-helper a-num (node-rtree a-bt) (cons 'right path))]
                      [else '()]))]
        (find-path-helper 4 SUBTREE3 '()))))

;; Tests using sample computations
(check-expect (find-path 30 SUBTREE1) SAMP-PATH1)
(check-expect (find-path 793 SUBTREE2) SAMP-PATH2)
(check-expect (find-path 4 SUBTREE3) SAMP-PATH3)

;; Tests using sample values
(check-error (find-path 10 (make-node 100 (make-node 50 (make-node 15 (make-node 11 '() '()) (make-node 17 '() '()))
                                          (make-node 60 (make-node 52 '() '()) (make-node 79 '() '())))
                            (make-node 150 (make-node 125 (make-node 110 '() '()) (make-node 145 '() '()))
                                       (make-node 175 (make-node 169 '() '()) (make-node 193 '() '()))))) "number not found in tree")
(check-expect (find-path 79 (make-node 90 (make-node 50 (make-node 45 (make-node 11 '() '()) (make-node 48 '() '()))
                                          (make-node 60 (make-node 52 '() '()) (make-node 79 '() '())))
                            (make-node 120 (make-node 115 (make-node 110 '() '()) (make-node 119 '() '()))
                                       (make-node 165 (make-node 145 '() '()) (make-node 193 '() '()))))) (list 'right 'right 'left))
(check-expect (find-path 15 (make-node 15 (make-node 10 (make-node 7 (make-node 3 '() '()) (make-node 9 '() '()))
                                          (make-node 20 (make-node 19 '() '()) (make-node 22 '() '())))
                            (make-node 73 (make-node 60 (make-node 50 '() '()) (make-node 63 '() '()))
                                       (make-node 115 (make-node 95 '() '()) (make-node 162 '() '()))))) "The given number is the root of the binary tree.")
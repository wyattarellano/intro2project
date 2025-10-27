#lang htdp/isl+
;; Design and implement a program to find the largest number in a (treeof number) using breadth-first search

;; A (treeof X) is either:
;;   1. '()
;;   2. (make-node X (listof node))
(define-struct node (val subtrees))

#| FUNCTION TEMPLATES 

;;  Sample (treeof X)
(define TOX0 '())
(define TOX1 (make-node ... ...))
     ...
;; (treeof X) ... --> ...
;; Purpose:
(define (f-on-tox a-tox ...)
  (if (empty? a-tox)
      ...
      (f-on-node a-tox ...)))

;; Sample expressions for f-on-tox
(define TOX0-VAL ...)
(define TOX1-VAL ...)
     ...
;; Tests using sample computations for f-on-tox
(check-expect (f-on-tox TOX0 ...) TOX0-VAL)
(check-expect (f-on-tox TOX1 ...) TOX1-VAL)
     ...
;; Tests using sample values for f-on-tox
(check-expect (f-on-tox ... ...) ...)
     ...

;; Sample nodes
(define NODE0 (make-node ... ...))
     ...
;; node ... --> ...
;; Purpose:
(define (f-on-node a-node ...)
  (...(f-on-X (node-val a-node ...)
   ...(f-on-lox (node-subtrees a-node) ...)))

;; Sample expressions for f-on-node
(define NODE0-VAL ...)
     ...
;; Tests using sample computations for f-on-node
(check-expect (f-on-node NODE0 ...) NODE0-VAL)
     ...
;; Tests using sample values for f-on-node
(check-expect (f-on-node ... ...) ...)
     ...


;; Sample (listof X)
(define LOX0 '())
(define LOX1 ...)
     ...
;; a-lox ... --> ...
;; Purpose:
(define (f-on-lox a-lox ...)
  (if (empty? a-lox)
      ...
      ... (f-on-X (first a-lox) ...)
          (f-on-lox (rest a-lox)) ...))

;; Sample expressions for f-on-lox
(define LOX0-VAL ...)
(define LOX1-VAL ...)
     ...
;; Tests using sample computations for f-on-lox
(check-expect (f-on-lox LOX0 ...) LOX0-VAL)
(check-expect (f-on-lox LOX1 ...) LOX1-VAL)
     ...
;; Tests using sample values for f-on-lox
(check-expect (f-on-lox ... ...) ...)
     ...
|#


;; A queue of X, (qof X), is either:
;;  1. empty
;;  2. (cons X (qof X))



(define RANDOM-NUM-RANGE 1000000) 
(define MAX-NUM-SUBTREES 10)

;; natnum --> (treeof number)
;; Purpose: Create a random tree of numbers of the given maximum depth
(define (make-tonatnum d)
  (local [(define root-val (random RANDOM-NUM-RANGE))]
    (cond  [(= d 0) (make-node root-val '())]
           [else
            (make-node root-val
                       (build-list (random MAX-NUM-SUBTREES)
                                   (λ (i) (make-tonatnum (sub1 d)))))])))

;; Sample expressions for make-tonatnum
(define TON0-1 (local [(define root-val (random RANDOM-NUM-RANGE))]
                 (make-node root-val '())))
(define TON0-2 (local [(define root-val (random RANDOM-NUM-RANGE))]
                 (make-node root-val '())))
(define TON1 (local [(define root-val (random RANDOM-NUM-RANGE))]
               (make-node root-val
                          (build-list (random MAX-NUM-SUBTREES)
                                      (λ (i) (make-tonatnum (sub1 1)))))))
(define TON2 (local [(define root-val (random RANDOM-NUM-RANGE))]
               (make-node root-val
                          (build-list (random MAX-NUM-SUBTREES)
                                      (λ (i) (make-tonatnum (sub1 2)))))))

;; Tests using sample computations for make-tonatnum
(check-satisfied TON0-1   (λ (t) (and (integer? (node-val t))
                                      (>= (node-val t) 0)
                                      (empty? (node-subtrees t)))))
(check-satisfied TON0-2 (λ (t) (and (integer? (node-val t))
                                    (>= (node-val t) 0)
                                    (empty? (node-subtrees t)))))
(check-satisfied
 TON1
 (λ (t) (and (integer? (node-val t))
             (>= (node-val t) 0)
             (< (length (node-subtrees t)) MAX-NUM-SUBTREES)
             (andmap (λ (n)
                       (and (integer? (node-val n))
                            (>= (node-val t) 0)))
                     (node-subtrees t))
             (andmap
              (λ (n) (empty? (node-subtrees n)))
              (node-subtrees t)))))

;; Tests using sample values for make-tonatnum
(check-satisfied (make-tonatnum 0)
                 (λ (t) (and (integer? (node-val t))
                             (>= (node-val t) 0)
                             (empty? (node-subtrees t)))))
(check-satisfied
 (make-node 7 '())
 (λ (t) (and (integer? (node-val t))
             (>= (node-val t) 0)
             (empty? (node-subtrees t)))))

                                  


(define (lon-max a-lonatnum)
  (if (empty? a-lonatnum)
      0
      (local [(define max-of-rest (lon-max (rest a-lonatnum)))]
        (if (> (first a-lonatnum) max-of-rest)
            (first a-lonatnum)
            max-of-rest))))

(define (tox-depth a-tox)
  (if (or (empty? a-tox) (empty? (node-subtrees a-tox)))
      0
      (add1 (lon-max (map tox-depth (node-subtrees a-tox))))))

(define NODE10  (make-node 10  '()))
(define NODE3   (make-node  3  '()))
(define NODE87  (make-node 87  '()))
(define NODE-5  (make-node  -5 '()))
(define NODE0   (make-node   0 '()))
(define NODE66  (make-node  66 '()))
(define NODE44  (make-node  44 '()))
(define NODE47  (make-node  47 '()))
(define NODE850 (make-node 850 (list NODE10 NODE3)))
(define NODE235 (make-node 235 (list NODE87 NODE-5 NODE0)))
(define NODE23  (make-node  23 (list NODE44 NODE47)))
(define NODE-88 (make-node -88 (list NODE23)))
(define NODE600 (make-node 600 (list NODE850 NODE235 NODE66 NODE-88)))

(define T0 '())
(define T1 NODE10)
(define T2 NODE600)
(define T3 (make-tonatnum 7)) ;; may take a long time to process

(check-satisfied T3 (λ (t) (<= (tox-depth t) 7)))
(check-satisfied (make-tonatnum 5)
                 (λ (t) (<= (tox-depth t) 5)))


(define E-QUEUE '())

(define qempty? empty?)

;; Tests for qempty?
(check-expect (qempty? '())      #true)
(check-expect (qempty? '(a b c)) #false)

;; (listof X) (qof X) --> (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (append a-qox a-lox))

;; Tests for enqueue
(check-expect (enqueue '(8 d) '()) '(8 d))
(check-expect (enqueue '(d) '(a b c)) '(a b c d))
(check-expect (enqueue '(6 5 4) '(7)) '(7 6 5 4))

;; (qof X) --> X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (first a-qox)))

;; Tests for qfirst
(check-error  (qfirst '()) "qfirst applied to an empty queue")
(check-expect (qfirst '(a b c)) 'a)

;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (rest a-qox)))

;; Tests for qfirst
(check-error  (dequeue '()) "dequeue applied to an empty queue")
(check-expect (dequeue '(a b c)) '(b c))

(define QTON0 '())
(define QTON1 (list T1))
(define QTON2 (list T2 T1))




;;(treeof number) -> number throws error
;;Purpose: To find the max number in a given (treeof number).
(define (find-max-bfs a-ton)
  (if (empty? a-ton)
      (error "there is no max for an empty treeof number")
      (local [;;(treeof numbers) (queueof (treeof numbers)) -> number
              ;;Purpose: To find the greatest number in a (treeof numbers).
              ;;ASSUMPTION: The given (treeof numbers) is not empty.
              
              ;;How: If the given queue is empty, it will return the root. Otherwise it will find the max
              ;;     of the (treeof number) by comparing the root to the recursive call on the subtrees.
              
              ;;Termination Argument: When the queue is qempty it will return the node value and terminate.
              ;;                      Otherwise for each recursive call the queue is dequeued and its
              ;;                      children are enqueued. This guarentees that all subtrees will be searched.
              ;;                      Therefore, the queue will eventually become qempty because there will be no
              ;;                      more subtrees to search, thus terminating. 
              (define (max-bfs-helper a-ton a-qoton)
                (if (qempty? a-qoton)
                    (node-val a-ton)
                    (max (node-val a-ton) (max-bfs-helper (qfirst a-qoton) (enqueue (node-subtrees (qfirst a-qoton)) (dequeue a-qoton))))))]

        (max-bfs-helper a-ton (node-subtrees a-ton)))))

;; sample expressions for find-max-bfs
(define SAMP-MAX-BFS-VAL1 (if (empty? TON1)
                              (error "there is no max for an empty treeof number")
                              (local [;;(treeof numbers) (queueof (treeof numbers)) -> number
                                      ;;Purpose: To find the greatest number in a (treeof numbers).
                                      ;;ASSUMPTION: The given (treeof numbers) is not empty.
              
                                      ;;How: If the given queue is empty, it will return the root. Otherwise it will find the max
                                      ;;     of the (treeof number) by comparing the root to the recursive call on the subtrees.
              
                                      ;;Termination Argument: When the queue is qempty it will return the node value and terminate.
                                      ;;                      Otherwise for each recursive call the queue is dequeued and its
                                      ;;                      children are enqueued. This guarentees that all subtrees will be searched.
                                      ;;                      Therefore, the queue will eventually become qempty because there will be no
                                      ;;                      more subtrees to search, thus terminating. 
                                      (define (max-bfs-helper a-ton a-qoton)
                                        (if (qempty? a-qoton)
                                            (node-val a-ton)
                                            (max (node-val a-ton) (max-bfs-helper (qfirst a-qoton) (enqueue (node-subtrees (qfirst a-qoton)) (dequeue a-qoton))))))]

                                (max-bfs-helper TON1 (node-subtrees TON1)))))

(define SAMP-MAX-BFS-VAL2 (if (empty? TON2)
                              (error "there is no max for an empty treeof number")
                              (local [;;(treeof numbers) (queueof (treeof numbers)) -> number
                                      ;;Purpose: To find the greatest number in a (treeof numbers).
                                      ;;ASSUMPTION: The given (treeof numbers) is not empty.
              
                                      ;;How: If the given queue is empty, it will return the root. Otherwise it will find the max
                                      ;;     of the (treeof number) by comparing the root to the recursive call on the subtrees.
              
                                      ;;Termination Argument: When the queue is qempty it will return the node value and terminate.
                                      ;;                      Otherwise for each recursive call the queue is dequeued and its
                                      ;;                      children are enqueued. This guarentees that all subtrees will be searched.
                                      ;;                      Therefore, the queue will eventually become qempty because there will be no
                                      ;;                      more subtrees to search, thus terminating. 
                                      (define (max-bfs-helper a-ton a-qoton)
                                        (if (qempty? a-qoton)
                                            (node-val a-ton)
                                            (max (node-val a-ton) (max-bfs-helper (qfirst a-qoton) (enqueue (node-subtrees (qfirst a-qoton)) (dequeue a-qoton))))))]

                                (max-bfs-helper TON2 (node-subtrees TON2)))))

(define SAMP-MAX-BFS-VAL3 (if (empty? TON0-1)
                              (error "there is no max for an empty treeof number")
                              (local [;;(treeof numbers) (queueof (treeof numbers)) -> number
                                      ;;Purpose: To find the greatest number in a (treeof numbers).
                                      ;;ASSUMPTION: The given (treeof numbers) is not empty.
              
                                      ;;How: If the given queue is empty, it will return the root. Otherwise it will find the max
                                      ;;     of the (treeof number) by comparing the root to the recursive call on the subtrees.
              
                                      ;;Termination Argument: When the queue is qempty it will return the node value and terminate.
                                      ;;                      Otherwise for each recursive call the queue is dequeued and its
                                      ;;                      children are enqueued. This guarentees that all subtrees will be searched.
                                      ;;                      Therefore, the queue will eventually become qempty because there will be no
                                      ;;                      more subtrees to search, thus terminating. 
                                      (define (max-bfs-helper a-ton a-qoton)
                                        (if (qempty? a-qoton)
                                            (node-val a-ton)
                                            (max (node-val a-ton) (max-bfs-helper (qfirst a-qoton) (enqueue (node-subtrees (qfirst a-qoton)) (dequeue a-qoton))))))]

                                (max-bfs-helper TON0-1 (node-subtrees TON0-1)))))


;; Tests using sample computations for find-max-bfs
(check-expect (find-max-bfs TON1) SAMP-MAX-BFS-VAL1)
(check-expect (find-max-bfs TON2) SAMP-MAX-BFS-VAL2)
(check-expect (find-max-bfs TON0-1) SAMP-MAX-BFS-VAL3)
(check-error (find-max-bfs T0) "there is no max for an empty treeof number")


;; Tests using sample values for find-max-bfs
(check-error (find-max-bfs '()) "there is no max for an empty treeof number")
(check-expect (find-max-bfs (make-node 123 '())) 123)
(check-expect (find-max-bfs (make-node 242 (list (make-node 122312 (list (make-node 347329847 (list (make-node 1894792 '())))))))) 347329847)
(check-expect (find-max-bfs (make-node 900917
                                       (list (make-node 107990 '())
                                             (make-node 893488 '())
                                             (make-node 966580 '())
                                             (make-node 305257 '())
                                             (make-node 172273 '())
                                             (make-node 50863 '())
                                             (make-node 190701 '())))) 966580)

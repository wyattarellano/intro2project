#lang htdp/isl+
;;(define SUBTREES (node-subtrees (qfirst a-queue)))
                    ;  (define DEQUEUE (dequeue a-queue))
              ;;(treeof number) queue accumulator -> number
              ;;Purpose: To find the max number in a given (treeof number)
              ;;ASSUMPTION: The given (treeof number) is not empty.
              ;;Accumulator Invariant: accum = the greatest value explored in the (treeof number) thus far
            #|  (define (find-max-bfs-helper a-ton a-queue accum)
                (cond [(qempty? a-queue)
                       accum]
                      [(> accum (node-val (qfirst a-queue)))
                       (find-max-bfs-helper (qfirst a-queue)
                                            (enqueue SUBTREES DEQUEUE)
                                            accum)]
                      [else (find-max-bfs-helper (qfirst a-queue)
                                                 (enqueue SUBTREES DEQUEUE)
                                                 (node-val (qfirst a-queue)))]))


|#
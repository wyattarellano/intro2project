#lang htdp/asl
#|

A bank is a (listof branches)

A branch is an interface that offers the following services:
1. init: -> (void)
2. add-account: string number -> (void)

A bank account is a structure, (make-account number client),
with  a balance and an owner.

A bank client is a structure, (make-client string (listof account)),
with a name and a list of accounts.

|#

(define-struct account (balance owner))

(define-struct client (name accounts))

;; (listof client)
;; Purpose: Store the clients of a bank
;; Assumption: Every client has a unique name
(define bank (void))

;;  --> (void)
;; Purpose: Initialize bank
(define (initialize-bank!)
  (set! bank '()))

;; --> branch
;;Purpose: To construct a branch
(define (make-branch)
  (local [;;loc
          ;;Purpose: To store value of a-loc
          (define loc (void))

          ;;loc -> (void)
          ;;Purpose: To reset a loc to empty
          (define (init!)
            (set! loc '()))
          
          ;; string number --> (void)
          ;; Purpose: To add an account for the given client with a given initial balance
          ;; Effect: A new client is added to the front of bank, if not already there,
          ;;         and a new account is added to the front of the client's
          ;;         accounts
          ;; Assumption: The given balance must be positive
          (define (add-account! name balance)
            (local [(define client-search (filter
                                           (λ (c) (string=? name (client-name c)))
                                           bank))

                    ;; --> client
                    ;; Purpose: To build a new client with an init-balance
                    (define (build-new-client)
                      (local [(define new-client (make-client name '()))]
                        (begin
                          (set-client-accounts! new-client (list (make-account balance new-client)))
                          new-client)))]
              (if (not (empty? client-search))
                  (local [(define the-client (first client-search))
                          (define new-acct (make-account balance the-client))]
                    (set-client-accounts! the-client (cons new-acct (client-accounts the-client))))
                  (set! loc (cons (build-new-client) loc)))))

          ; Message --> branch throws error
          ; Purpose: To manage a-loc
          (define (branch-object a-message)
            (cond [(eq? a-message 'add-account) add-account!]
                  [(eq? a-message 'clients) loc]
                  [(eq? a-message 'initialize) init!]
                  [else (error 'branch-object
                               (format "Unknown message received: ~s"
                                       a-message))]))]
    branch-object))
              


;; Sample branches
(define BRANCH1 (make-branch))
(define BRANCH2 (make-branch))
(define BRANCH3 (make-branch))

;; branch --> loc
;; Purpose: to get the clients in a branch
(define (get-branch branch) (branch 'clients))

;; branch --> loc
;; Purpose: To initialize a branch
(define (init-branch branch) ((branch 'initialize)))

;;branch --> loc
;; Purpose: To add an account to a branch
(define (branch-account name balance branch) ((branch 'add-account) name balance))

;;Tests
;;As you can see, the accounts made using the same name, do not affect other accounts
;;in other branches. This means, that our tests illustrate that changes to one
;;branch do not affect a different branch.
(check-expect (begin (initialize-bank!)
                     (init-branch BRANCH1)
                     (init-branch BRANCH2)
                     (init-branch BRANCH3)
                     (branch-account "James" 500 BRANCH1)
                     (branch-account "James" 200 BRANCH2)
                     (branch-account "James" 800 BRANCH3)
                     (branch-account "Wyatt" 300 BRANCH1)
                     (branch-account "Wyatt" 2500 BRANCH2)
                     (branch-account "Wyatt" 50 BRANCH3)
                     (branch-account "Sophia" 5000 BRANCH1)
                     (branch-account "Sophia" 524 BRANCH2)
                     (branch-account "Sophia" 861 BRANCH3)
                     (get-branch BRANCH1))
              (shared ((-1- (make-client "Sophia" (list (make-account 5000 -1-))))
                       (-11- (make-client "James" (list (make-account 500 -11-))))
                       (-6- (make-client "Wyatt" (list (make-account 300 -6-))))) (list -1- -6- -11-)))

(check-expect (begin (initialize-bank!)
                     (init-branch BRANCH1)
                     (init-branch BRANCH2)
                     (init-branch BRANCH3)
                     (branch-account "James" 500 BRANCH1)
                     (branch-account "James" 200 BRANCH2)
                     (branch-account "James" 800 BRANCH3)
                     (branch-account "Wyatt" 300 BRANCH1)
                     (branch-account "Wyatt" 2500 BRANCH2)
                     (branch-account "Wyatt" 50 BRANCH3)
                     (branch-account "Sophia" 5000 BRANCH1)
                     (branch-account "Sophia" 524 BRANCH2)
                     (branch-account "Sophia" 861 BRANCH3)
                     (get-branch BRANCH2))
              (shared ((-1- (make-client "Sophia" (list (make-account 524 -1-))))
                       (-11- (make-client "James" (list (make-account 200 -11-))))
                       (-6- (make-client "Wyatt" (list (make-account 2500 -6-))))) (list -1- -6- -11-)))

(check-expect (begin (initialize-bank!)
                     (init-branch BRANCH1)
                     (init-branch BRANCH2)
                     (init-branch BRANCH3)
                     (branch-account "James" 500 BRANCH1)
                     (branch-account "James" 200 BRANCH2)
                     (branch-account "James" 800 BRANCH3)
                     (branch-account "Wyatt" 300 BRANCH1)
                     (branch-account "Wyatt" 2500 BRANCH2)
                     (branch-account "Wyatt" 50 BRANCH3)
                     (branch-account "Sophia" 5000 BRANCH1)
                     (branch-account "Sophia" 524 BRANCH2)
                     (branch-account "Sophia" 861 BRANCH3)
                     (get-branch BRANCH3))
              (shared ((-1- (make-client "Sophia" (list (make-account 861 -1-))))
                       (-11- (make-client "James" (list (make-account 800 -11-))))
                       (-6- (make-client "Wyatt" (list (make-account 50 -6-))))) (list -1- -6- -11-)))
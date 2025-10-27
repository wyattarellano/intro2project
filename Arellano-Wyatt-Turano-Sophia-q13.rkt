#lang htdp/asl

#|
A bank is an interface with a (listof branch) that offers the following services:
    1. 'init
    2. 'add-branch
    3. 'bank->list
    1. 'create-account: string number branch --> (void)

TEMPLATE FOR bank CLASS
;;  --> bank
;; Purpose: To contruct an branch
(define (make-bank)
  (local [;; number >= 0
          ;; Purpose: Store the balance of the bank account
          (define mybalance 'uninitialized)

          ;; number --> (void) throws error
          ;; Purpose: Initialize mybalance
          ;; Effect: mybalance is mutated to the given number
          (define (initialize-mybalance! init-balance) ...)

          ;; number --> (void) throws error
          ;; Purpose: To make a deposit
          ;; Effect: The given amount is added to mybalance
          (define (deposit! amt) ...)

          ;; number --> (void) throws error
          ;; Purpose: To make a withdrawal
          ;; Effect: The given amount is subtracted from mybalance
          (define (withdraw! amt) ...)

          ;;  --> number
          ;; Purpose: Return the current balance
          (define (get-balance) ...)

          ;; message --> ba 
          ;; Purpose: To manage branch service
          (define (branch-object a-message)
            (cond [(eq? a-message 'create-account) create-account!]
                  [else (error 'branch-object (format "Unknown message received: ~s" a-message))]))]
      branch-object))

A branch is a bank that is a (listof clients)

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

;;  --> (listof (cons string (listof number)))
;; Purpose: Tranform bank information into a list
(define (bank->list)
  (map (λ (client)
         (cons (client-name client)
               (map (λ (acct) (account-balance acct))
                    (client-accounts client))))
       bank))

;; Tests for bank->list
(check-expect (begin
                (initialize-bank!)
                (bank->list))
              '())

(check-expect (begin
                (set! bank (shared ((-1- (make-client
                                           "Frances Allen"
                                           (list (make-account 500 -1-)))))
                             (list -1-)))
                (bank->list))
              (list (list "Frances Allen" 500)))

#|
(check-expect (begin
                (initialize-bank!)
                (set! bank (list 
                (bank->list))
              '())
|#

;; build-new-client before encapslation

;; string number --> client
;; Purpose: To build a new client with given name and a single new 
;;          account that has the given balance
;; Assumption: The given initial balance is positive
(define (build-new-client name init-balance)
  (local [(define new-client (make-client name '()))]
    (begin
      (set-client-accounts! new-client (list (make-account init-balance new-client)))
      new-client)))

;; Sample expressions for build-new-client
(define HOPPER (local [(define new-client (make-client "Grace Hopper" '()))]
                 (begin
                   (set-client-accounts! new-client
                                         (list (make-account 847 new-client)))
                   new-client)))

(define RHODES  (local [(define new-client (make-client "Ida Rhodes" '()))]
                  (begin
                    (set-client-accounts! new-client
                                          (list (make-account 1301 new-client)))
                    new-client)))

;; Tests using sample computations for build-new-client
(check-expect (build-new-client "Grace Hopper" 847)  HOPPER)
(check-expect (build-new-client "Ida Rhodes"   1301) RHODES)

;;  Tests using sample values for build-new-client
(check-expect (build-new-client "Ada Lovelace" 1000)
              (shared ((-0- (make-client "Ada Lovelace"
                                         (list (make-account 1000 -0-)))))
                -0-))

(check-expect (build-new-client "Mary Kenneth Keller" 431)
              (shared ((-0- (make-client "Mary Kenneth Keller"
                                         (list (make-account 431 -0-)))))
                -0-))


;; string number --> (void)
;; Purpose: Add an account for the given client name with the given
;;          initial balance
;; Effect: A new client, if necessary, is added to the front of bank
;;         and a new account is added to the front of the client's
;;         accounts
;; Assumption: The given balance is positive
(define (add-account! name balance)
  (local [(define client-search (filter
                                 (λ (c) (string=? name (client-name c)))
                                 bank))

          ;;  --> client
          ;; Purpose: To build a new client with name and a single new 
          ;;          account that has init-balance
          (define (build-new-client)
            (local [(define new-client (make-client name '()))]
              (begin
                (set-client-accounts! new-client (list (make-account balance new-client)))
                new-client)))]
    (if (not (empty? client-search))
        (local [(define the-client (first client-search))
                (define new-acct (make-account balance the-client))]
          (set-client-accounts! the-client (cons new-acct (client-accounts the-client))))
        (set! bank (cons (build-new-client) bank)))))

(check-expect (begin
                (initialize-bank!)
                (bank->list))
              '())

(check-expect (begin
                (initialize-bank!)
                (add-account! "Joan Clarke" 1000)
                (bank->list))
              (list (list "Joan Clarke" 1000)))

(check-expect (begin
                (initialize-bank!)
                (add-account! "Joan Clarke" 1000)
                (add-account! "Katherine Johnson" 100)
                (add-account! "Joan Clarke" 6500)
                (add-account! "Jean E. Sammet " 830)
                (bank->list))
              (list (list "Jean E. Sammet " 830)
                    (list "Katherine Johnson" 100)
                    (list "Joan Clarke" 6500 1000)))
              


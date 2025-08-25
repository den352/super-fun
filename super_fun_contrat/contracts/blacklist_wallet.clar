;; Blacklist Wallet Smart Contract
;; This contract manages a blacklist of addresses and prevents blacklisted users from depositing or withdrawing

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_BLACKLISTED (err u403))
(define-constant ERR_INSUFFICIENT_BALANCE (err u404))
(define-constant ERR_INVALID_AMOUNT (err u400))

;; Contract owner (deployer)
(define-constant CONTRACT_OWNER tx-sender)

;; Data variables
(define-data-var total-deposits uint u0)

;; Data maps
;; Track blacklisted addresses
(define-map blacklisted-addresses principal bool)

;; Track user balances
(define-map user-balances principal uint)

;; Private functions

;; Check if a user is blacklisted
(define-private (is-blacklisted (user principal))
    (default-to false (map-get? blacklisted-addresses user))
)

;; Public functions

;; Add an address to the blacklist (only contract owner can call this)
(define-public (add-to-blacklist (user principal))
    (begin
        ;; Check if caller is the contract owner
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        ;; Add user to blacklist
        (map-set blacklisted-addresses user true)
        
        (ok true)
    )
)

;; Remove an address from the blacklist (only contract owner can call this)
(define-public (remove-from-blacklist (user principal))
    (begin
        ;; Check if caller is the contract owner
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        ;; Remove user from blacklist
        (map-delete blacklisted-addresses user)
        
        (ok true)
    )
)

;; Deposit STX tokens (blocked if blacklisted)
(define-public (deposit (amount uint))
    (let (
        (current-balance (default-to u0 (map-get? user-balances tx-sender)))
    )
        ;; Check if amount is valid
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        
        ;; Check if user is not blacklisted
        (asserts! (not (is-blacklisted tx-sender)) ERR_BLACKLISTED)
        
        ;; Transfer STX from user to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update user balance
        (map-set user-balances tx-sender (+ current-balance amount))
        
        ;; Update total deposits
        (var-set total-deposits (+ (var-get total-deposits) amount))
        
        (ok amount)
    )
)

;; Withdraw STX tokens (blocked if blacklisted)
(define-public (withdraw (amount uint))
    (let (
        (current-balance (default-to u0 (map-get? user-balances tx-sender)))
    )
        ;; Check if amount is valid
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        
        ;; Check if user is not blacklisted
        (asserts! (not (is-blacklisted tx-sender)) ERR_BLACKLISTED)
        
        ;; Check if user has sufficient balance
        (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
        
        ;; Update user balance
        (map-set user-balances tx-sender (- current-balance amount))
        
        ;; Update total deposits
        (var-set total-deposits (- (var-get total-deposits) amount))
        
        ;; Transfer STX from contract to user
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        
        (ok amount)
    )
)

;; Emergency withdrawal by contract owner (bypasses blacklist for contract owner only)
(define-public (emergency-withdraw (user principal) (amount uint))
    (let (
        (current-balance (default-to u0 (map-get? user-balances user)))
    )
        ;; Check if caller is the contract owner
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        
        ;; Check if amount is valid
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        
        ;; Check if user has sufficient balance
        (asserts! (>= current-balance amount) ERR_INSUFFICIENT_BALANCE)
        
        ;; Update user balance
        (map-set user-balances user (- current-balance amount))
        
        ;; Update total deposits
        (var-set total-deposits (- (var-get total-deposits) amount))
        
        ;; Transfer STX from contract to user
        (try! (as-contract (stx-transfer? amount tx-sender user)))
        
        (ok amount)
    )
)

;; Read-only functions

;; Check if an address is blacklisted
(define-read-only (check-blacklist-status (user principal))
    (is-blacklisted user)
)

;; Get user balance
(define-read-only (get-balance (user principal))
    (default-to u0 (map-get? user-balances user))
)

;; Get total deposits in the contract
(define-read-only (get-total-deposits)
    (var-get total-deposits)
)

;; Get contract owner
(define-read-only (get-contract-owner)
    CONTRACT_OWNER
)

;; Get contract's STX balance
(define-read-only (get-contract-stx-balance)
    (stx-get-balance (as-contract tx-sender))
)
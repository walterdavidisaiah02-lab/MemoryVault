;; title: token-vault
;; version: 1.0.0
;; summary: A secure vault contract for depositing and withdrawing STX tokens
;; description: This contract allows users to deposit STX tokens into a vault,
;; track their balances, and withdraw their funds at any time. It includes
;; emergency pause functionality and comprehensive balance tracking.

;; traits
;;

;; token definitions
;;

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-contract-paused (err u103))
(define-constant err-transfer-failed (err u104))
(define-constant err-already-paused (err u105))
(define-constant err-not-paused (err u106))

;; data vars
;;
(define-data-var contract-paused bool false)
(define-data-var total-deposits uint u0)

;; data maps
;;
(define-map user-balances principal uint)
(define-map deposit-history 
  { user: principal, deposit-id: uint }
  { amount: uint, block-height: uint, timestamp: uint }
)
(define-map user-deposit-count principal uint)

;; public functions
;;

;; Deposit STX into the vault
(define-public (deposit (amount uint))
  (let
    (
      (sender tx-sender)
      (current-balance (default-to u0 (map-get? user-balances sender)))
      (deposit-count (default-to u0 (map-get? user-deposit-count sender)))
    )
    ;; Validate inputs
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer STX from sender to contract
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    ;; Update user balance
    (map-set user-balances sender (+ current-balance amount))
    
    ;; Record deposit history
    (map-set deposit-history
      { user: sender, deposit-id: deposit-count }
      { 
        amount: amount, 
        block-height: stacks-block-height,
        timestamp: stacks-block-height ;; Using block height as timestamp proxy
      }
    )
    
    ;; Update deposit count
    (map-set user-deposit-count sender (+ deposit-count u1))
    
    ;; Update total deposits
    (var-set total-deposits (+ (var-get total-deposits) amount))
    
    (ok amount)
  )
)

;; Withdraw STX from the vault
(define-public (withdraw (amount uint))
  (let
    (
      (sender tx-sender)
      (current-balance (default-to u0 (map-get? user-balances sender)))
    )
    ;; Validate inputs
    (asserts! (not (var-get contract-paused)) err-contract-paused)
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (>= current-balance amount) err-insufficient-balance)
    
    ;; Update user balance first (checks-effects-interactions pattern)
    (map-set user-balances sender (- current-balance amount))
    
    ;; Update total deposits
    (var-set total-deposits (- (var-get total-deposits) amount))
    
    ;; Transfer STX from contract to sender
    (match (as-contract (stx-transfer? amount tx-sender sender))
      success (ok amount)
      error err-transfer-failed
    )
  )
)

;; Emergency pause function (owner only)
(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (var-get contract-paused)) err-already-paused)
    (var-set contract-paused true)
    (ok true)
  )
)

;; Unpause contract (owner only)
(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (var-get contract-paused) err-not-paused)
    (var-set contract-paused false)
    (ok true)
  )
)

;; read only functions
;;

;; Get user balance
(define-read-only (get-balance (user principal))
  (ok (default-to u0 (map-get? user-balances user)))
)

;; Get contract pause status
(define-read-only (is-paused)
  (ok (var-get contract-paused))
)

;; Get total deposits in vault
(define-read-only (get-total-deposits)
  (ok (var-get total-deposits))
)

;; Get deposit history for a user
(define-read-only (get-deposit-history (user principal) (deposit-id uint))
  (ok (map-get? deposit-history { user: user, deposit-id: deposit-id }))
)

;; Get user's total number of deposits
(define-read-only (get-deposit-count (user principal))
  (ok (default-to u0 (map-get? user-deposit-count user)))
)

;; Check if caller is contract owner
(define-read-only (is-contract-owner (caller principal))
  (ok (is-eq caller contract-owner))
)

;; private functions
;;
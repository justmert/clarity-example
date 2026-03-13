;; ClariFi Staking Rewards
;; Stake CLFI tokens to earn protocol revenue share.
;; Rewards are distributed proportionally based on stake weight.

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u4000))
(define-constant ERR-INVALID-AMOUNT (err u4001))
(define-constant ERR-NOTHING-TO-CLAIM (err u4002))
(define-constant ERR-INSUFFICIENT-STAKE (err u4003))

;; Precision multiplier for reward-per-share calculations
(define-constant PRECISION u1000000000000)

;; ---- Global State ----

(define-data-var total-staked uint u0)
(define-data-var reward-per-share uint u0)
(define-data-var total-distributed uint u0)
(define-data-var rewards-paused bool false)

;; ---- Per-User State ----

;; Amount of CLFI staked by each user
(define-map stakes principal uint)

;; Reward debt: tracks how much reward has been accounted for at time of stake
(define-map reward-debt principal uint)

;; Cumulative rewards claimed by each user
(define-map claimed-rewards principal uint)

;; ---- Read-Only Functions ----

(define-read-only (get-stake (user principal))
  (default-to u0 (map-get? stakes user)))

(define-read-only (get-total-staked)
  (ok (var-get total-staked)))

(define-read-only (get-reward-info)
  (ok {
    reward-per-share: (var-get reward-per-share),
    total-distributed: (var-get total-distributed),
    total-staked: (var-get total-staked)
  }))

;; Calculate pending (unclaimed) rewards for a user
(define-read-only (get-pending-reward (user principal))
  (let (
    (user-stake (get-stake user))
    (accumulated (/ (* user-stake (var-get reward-per-share)) PRECISION))
    (debt (default-to u0 (map-get? reward-debt user)))
  )
    (if (>= accumulated debt)
      (ok (- accumulated debt))
      (ok u0))))

(define-read-only (get-claimed (user principal))
  (ok (default-to u0 (map-get? claimed-rewards user))))

;; ---- Staking Functions ----

;; Stake CLFI tokens into the rewards pool
(define-public (stake (amount uint))
  (let (
    (user tx-sender)
    (current-stake (get-stake user))
    (new-stake (+ current-stake amount))
  )
    (asserts! (not (var-get rewards-paused)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Transfer CLFI tokens from user to this contract
    (try! (contract-call? .clarifi-token transfer amount user (as-contract tx-sender) none))
    ;; Update user's stake and reward tracking
    (map-set stakes user new-stake)
    (map-set reward-debt user
      (/ (* new-stake (var-get reward-per-share)) PRECISION))
    (var-set total-staked (+ (var-get total-staked) amount))
    (print { event: "stake", user: user, amount: amount, total: new-stake })
    (ok true)))

;; Remove staked CLFI tokens from the pool
(define-public (unstake (amount uint))
  (let (
    (user tx-sender)
    (current-stake (get-stake user))
    (new-stake (- current-stake amount))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount current-stake) ERR-INSUFFICIENT-STAKE)
    ;; Settle any pending rewards first
    (try! (settle-rewards user))
    ;; Update user's stake
    (map-set stakes user new-stake)
    (map-set reward-debt user
      (/ (* new-stake (var-get reward-per-share)) PRECISION))
    (var-set total-staked (- (var-get total-staked) amount))
    ;; Return CLFI tokens to user
    (try! (as-contract (contract-call? .clarifi-token transfer amount tx-sender user none)))
    (print { event: "unstake", user: user, amount: amount, remaining: new-stake })
    (ok true)))

;; Claim accumulated staking rewards (paid in STX)
(define-public (claim)
  (settle-rewards tx-sender))

;; ---- Reward Distribution ----

;; Distribute STX rewards to the staking pool (called by protocol or admin)
(define-public (distribute-rewards (amount uint))
  (let (
    (current-total (var-get total-staked))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Transfer reward STX into this contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    ;; Update reward accumulator
    (if (> current-total u0)
      (begin
        (var-set reward-per-share
          (+ (var-get reward-per-share) (/ (* amount PRECISION) current-total)))
        (var-set total-distributed (+ (var-get total-distributed) amount)))
      true)
    (print { event: "distribute", amount: amount, total-staked: current-total })
    (ok true)))

;; ---- Internal ----

;; Settle and transfer pending rewards for a user
(define-private (settle-rewards (user principal))
  (let (
    (user-stake (get-stake user))
    (accumulated (/ (* user-stake (var-get reward-per-share)) PRECISION))
    (debt (default-to u0 (map-get? reward-debt user)))
    (pending (if (>= accumulated debt) (- accumulated debt) u0))
  )
    (if (> pending u0)
      (begin
        (try! (as-contract (stx-transfer? pending tx-sender user)))
        (map-set reward-debt user accumulated)
        (map-set claimed-rewards user
          (+ (default-to u0 (map-get? claimed-rewards user)) pending))
        (print { event: "claim", user: user, amount: pending })
        (ok true))
      (ok true))))

;; ---- Admin Functions ----

(define-public (set-rewards-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set rewards-paused paused))))

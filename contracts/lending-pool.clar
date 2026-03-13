;; ClariFi Lending Pool
;; Core lending protocol: deposit STX collateral, borrow against it,
;; repay loans, and liquidate undercollateralized positions.

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u2000))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u2001))
(define-constant ERR-INSUFFICIENT-BALANCE (err u2002))
(define-constant ERR-POSITION-HEALTHY (err u2003))
(define-constant ERR-INVALID-AMOUNT (err u2004))
(define-constant ERR-NO-POSITION (err u2005))
(define-constant ERR-POOL-PAUSED (err u2006))
(define-constant ERR-ZERO-DEBT (err u2007))

;; Protocol parameters (expressed as percentages, e.g., u150 = 150%)
(define-constant MIN-COLLATERAL-RATIO u150)
(define-constant LIQUIDATION-THRESHOLD u120)
(define-constant LIQUIDATION-BONUS u5)
(define-constant PROTOCOL-FEE-BPS u30) ;; 0.3% protocol fee on borrows

;; ---- Pool State ----

(define-data-var total-collateral uint u0)
(define-data-var total-borrows uint u0)
(define-data-var total-reserves uint u0)
(define-data-var pool-paused bool false)
(define-data-var interest-rate-per-block uint u1) ;; simplified per-block rate in basis points

;; ---- User Position Storage ----

(define-map user-collateral principal uint)
(define-map user-borrows principal uint)
(define-map last-interest-block principal uint)

;; ---- Read-Only Functions ----

(define-read-only (get-collateral-balance (user principal))
  (default-to u0 (map-get? user-collateral user)))

(define-read-only (get-borrow-balance (user principal))
  (default-to u0 (map-get? user-borrows user)))

(define-read-only (get-pool-state)
  (ok {
    total-collateral: (var-get total-collateral),
    total-borrows: (var-get total-borrows),
    total-reserves: (var-get total-reserves),
    interest-rate: (var-get interest-rate-per-block),
    paused: (var-get pool-paused)
  }))

;; Calculate the health factor for a user's position
;; Returns a percentage value: >= 150 is healthy, < 120 is liquidatable
(define-read-only (get-health-factor (user principal))
  (let (
    (collateral (get-collateral-balance user))
    (debt (get-borrow-balance user))
    (price (unwrap-panic (contract-call? .price-oracle get-stx-price)))
    (collateral-value (/ (* collateral price) u1000000))
  )
    (if (is-eq debt u0)
      (ok u9999)
      (ok (/ (* collateral-value u100) debt)))))

;; ---- Core Functions ----

;; Deposit STX as collateral into the lending pool
(define-public (deposit-collateral (amount uint))
  (let (
    (user tx-sender)
    (current (get-collateral-balance user))
  )
    (asserts! (not (var-get pool-paused)) ERR-POOL-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (stx-transfer? amount user (as-contract tx-sender)))
    (map-set user-collateral user (+ current amount))
    (var-set total-collateral (+ (var-get total-collateral) amount))
    (print { event: "deposit", user: user, amount: amount })
    (ok true)))

;; Withdraw collateral (must maintain health factor)
(define-public (withdraw-collateral (amount uint))
  (let (
    (user tx-sender)
    (current-collateral (get-collateral-balance user))
    (current-debt (get-borrow-balance user))
    (remaining (- current-collateral amount))
    (price (unwrap-panic (contract-call? .price-oracle get-stx-price)))
    (remaining-value (/ (* remaining price) u1000000))
  )
    (asserts! (not (var-get pool-paused)) ERR-POOL-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount current-collateral) ERR-INSUFFICIENT-BALANCE)
    ;; Verify position stays healthy after withdrawal
    (if (> current-debt u0)
      (asserts! (>= (/ (* remaining-value u100) current-debt) MIN-COLLATERAL-RATIO)
        ERR-INSUFFICIENT-COLLATERAL)
      true)
    (map-set user-collateral user remaining)
    (var-set total-collateral (- (var-get total-collateral) amount))
    (try! (as-contract (stx-transfer? amount tx-sender user)))
    (print { event: "withdraw", user: user, amount: amount })
    (ok true)))

;; Borrow against deposited collateral
(define-public (borrow (amount uint))
  (let (
    (user tx-sender)
    (collateral (get-collateral-balance user))
    (current-debt (get-borrow-balance user))
    (fee (/ (* amount PROTOCOL-FEE-BPS) u10000))
    (total-debt (+ current-debt amount fee))
    (price (unwrap-panic (contract-call? .price-oracle get-stx-price)))
    (collateral-value (/ (* collateral price) u1000000))
  )
    (asserts! (not (var-get pool-paused)) ERR-POOL-PAUSED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Verify sufficient collateralization after borrow
    (asserts! (>= (/ (* collateral-value u100) total-debt) MIN-COLLATERAL-RATIO)
      ERR-INSUFFICIENT-COLLATERAL)
    (map-set user-borrows user total-debt)
    (map-set last-interest-block user block-height)
    (var-set total-borrows (+ (var-get total-borrows) amount))
    (var-set total-reserves (+ (var-get total-reserves) fee))
    (try! (as-contract (stx-transfer? amount tx-sender user)))
    (print { event: "borrow", user: user, amount: amount, fee: fee })
    (ok true)))

;; Repay borrowed amount (partial or full)
(define-public (repay (amount uint))
  (let (
    (user tx-sender)
    (current-debt (get-borrow-balance user))
    (repay-amount (if (> amount current-debt) current-debt amount))
  )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> current-debt u0) ERR-NO-POSITION)
    (try! (stx-transfer? repay-amount user (as-contract tx-sender)))
    (map-set user-borrows user (- current-debt repay-amount))
    (var-set total-borrows (- (var-get total-borrows) repay-amount))
    (print { event: "repay", user: user, amount: repay-amount })
    (ok repay-amount)))

;; Liquidate an undercollateralized position
(define-public (liquidate (borrower principal) (repay-amount uint))
  (let (
    (liquidator tx-sender)
    (borrower-collateral (get-collateral-balance borrower))
    (borrower-debt (get-borrow-balance borrower))
    (price (unwrap-panic (contract-call? .price-oracle get-stx-price)))
    (collateral-value (/ (* borrower-collateral price) u1000000))
    (health (/ (* collateral-value u100) borrower-debt))
    ;; Liquidator receives repay value plus bonus percentage as collateral
    (seize-value (+ repay-amount (/ (* repay-amount LIQUIDATION-BONUS) u100)))
    (seize-collateral (/ (* seize-value u1000000) price))
  )
    (asserts! (not (var-get pool-paused)) ERR-POOL-PAUSED)
    (asserts! (> repay-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> borrower-debt u0) ERR-NO-POSITION)
    ;; Position must be below liquidation threshold
    (asserts! (< health LIQUIDATION-THRESHOLD) ERR-POSITION-HEALTHY)
    ;; Transfer repayment from liquidator into pool
    (try! (stx-transfer? repay-amount liquidator (as-contract tx-sender)))
    ;; Reduce borrower's debt
    (map-set user-borrows borrower (- borrower-debt repay-amount))
    (var-set total-borrows (- (var-get total-borrows) repay-amount))
    ;; Transfer seized collateral to liquidator
    (map-set user-collateral borrower (- borrower-collateral seize-collateral))
    (var-set total-collateral (- (var-get total-collateral) seize-collateral))
    (try! (as-contract (stx-transfer? seize-collateral tx-sender liquidator)))
    (print {
      event: "liquidation",
      liquidator: liquidator,
      borrower: borrower,
      repaid: repay-amount,
      seized: seize-collateral
    })
    (ok { repaid: repay-amount, seized: seize-collateral })))

;; Accrue interest on a user's borrow position
(define-public (accrue-interest (user principal))
  (let (
    (debt (get-borrow-balance user))
    (last-block (default-to block-height (map-get? last-interest-block user)))
    (blocks-elapsed (- block-height last-block))
    (interest (/ (* debt (var-get interest-rate-per-block) blocks-elapsed) u10000))
  )
    (if (and (> debt u0) (> blocks-elapsed u0))
      (begin
        (map-set user-borrows user (+ debt interest))
        (map-set last-interest-block user block-height)
        (var-set total-borrows (+ (var-get total-borrows) interest))
        (ok interest))
      (ok u0))))

;; ---- Admin Functions ----

(define-public (set-pool-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set pool-paused paused)
    (print { event: "pool-pause-toggle", paused: paused })
    (ok true)))

(define-public (set-interest-rate (rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set interest-rate-per-block rate)
    (ok true)))

(define-public (withdraw-reserves (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount (var-get total-reserves)) ERR-INSUFFICIENT-BALANCE)
    (var-set total-reserves (- (var-get total-reserves) amount))
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok true)))

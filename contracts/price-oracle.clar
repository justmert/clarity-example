;; ClariFi Price Oracle
;; Aggregated price feed for STX/USD used by the lending pool.
;; Authorized reporters submit prices; the contract tracks the latest value.

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u3000))
(define-constant ERR-INVALID-PRICE (err u3001))
(define-constant ERR-REPORTER-EXISTS (err u3002))
(define-constant ERR-NO-PRICE (err u3003))

;; Price is stored as fixed-point with 8 decimals (e.g., 150000000 = $1.50)
(define-data-var stx-price uint u0)
(define-data-var price-updated-at uint u0)
(define-data-var reporter-count uint u0)

;; Map of authorized price reporters
(define-map reporters principal bool)

;; Each reporter's most recent price submission
(define-map price-submissions principal { price: uint, submitted-at: uint })

;; Initialize deployer as first reporter
(map-set reporters CONTRACT-OWNER true)
(var-set reporter-count u1)

;; ---- Read-Only Functions ----

(define-read-only (get-stx-price)
  (let ((price (var-get stx-price)))
    (asserts! (> price u0) ERR-NO-PRICE)
    (ok price)))

(define-read-only (get-price-info)
  (ok {
    price: (var-get stx-price),
    updated-at: (var-get price-updated-at),
    reporters: (var-get reporter-count)
  }))

(define-read-only (get-price-age)
  (- block-height (var-get price-updated-at)))

(define-read-only (is-reporter (who principal))
  (default-to false (map-get? reporters who)))

(define-read-only (get-submission (who principal))
  (map-get? price-submissions who))

;; ---- Reporter Functions ----

;; Submit a new price observation
(define-public (submit-price (price uint))
  (begin
    (asserts! (is-reporter tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> price u0) ERR-INVALID-PRICE)
    ;; Record this reporter's submission
    (map-set price-submissions tx-sender { price: price, submitted-at: block-height })
    ;; Update the canonical price
    (var-set stx-price price)
    (var-set price-updated-at block-height)
    (print { event: "price-update", reporter: tx-sender, price: price, block: block-height })
    (ok price)))

;; ---- Admin Functions ----

(define-public (add-reporter (who principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (is-reporter who)) ERR-REPORTER-EXISTS)
    (map-set reporters who true)
    (var-set reporter-count (+ (var-get reporter-count) u1))
    (print { event: "reporter-added", reporter: who })
    (ok true)))

(define-public (remove-reporter (who principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-reporter who) ERR-NOT-AUTHORIZED)
    (map-set reporters who false)
    (var-set reporter-count (- (var-get reporter-count) u1))
    (print { event: "reporter-removed", reporter: who })
    (ok true)))

;; Owner override for emergency price corrections
(define-public (set-price-direct (price uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> price u0) ERR-INVALID-PRICE)
    (var-set stx-price price)
    (var-set price-updated-at block-height)
    (print { event: "price-override", price: price })
    (ok true)))

;; ClariFi Token (CLFI)
;; SIP-010 compliant fungible token for the ClariFi lending protocol.
;; Used for governance voting, staking rewards, and fee capture.

(define-fungible-token clarifi)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INVALID-AMOUNT (err u1001))
(define-constant ERR-ZERO-ADDRESS (err u1002))

(define-data-var token-name (string-ascii 32) "ClariFi Token")
(define-data-var token-symbol (string-ascii 10) "CLFI")
(define-data-var token-uri (optional (string-utf8 256)) none)

;; Authorized contracts that can mint/burn on behalf of the protocol
(define-map authorized-minters principal bool)

;; Track total minted per minter for transparency
(define-map minter-minted principal uint)

;; Initialize deployer as first minter
(map-set authorized-minters CONTRACT-OWNER true)

;; ---- SIP-010 Standard Functions ----

(define-read-only (get-name)
  (ok (var-get token-name)))

(define-read-only (get-symbol)
  (ok (var-get token-symbol)))

(define-read-only (get-decimals)
  (ok u6))

(define-read-only (get-balance (account principal))
  (ok (ft-get-balance clarifi account)))

(define-read-only (get-total-supply)
  (ok (ft-get-supply clarifi)))

(define-read-only (get-token-uri)
  (ok (var-get token-uri)))

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (match memo m (print m) 0x)
    (ft-transfer? clarifi amount sender recipient)))

;; ---- Minting ----

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-authorized tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (map-set minter-minted tx-sender
      (+ (default-to u0 (map-get? minter-minted tx-sender)) amount))
    (ft-mint? clarifi amount recipient)))

;; ---- Burning ----

(define-public (burn (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (ft-burn? clarifi amount tx-sender)))

;; Protocol-level burn used by lending pool for repayments and liquidations
(define-public (burn-from (amount uint) (account principal))
  (begin
    (asserts! (is-authorized contract-caller) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (ft-burn? clarifi amount account)))

;; ---- Minter Administration ----

(define-public (add-minter (who principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-minters who true))))

(define-public (revoke-minter (who principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (map-set authorized-minters who false))))

(define-read-only (is-minter (who principal))
  (ok (is-authorized who)))

;; ---- URI Management ----

(define-public (set-token-uri (uri (optional (string-utf8 256))))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (ok (var-set token-uri uri))))

;; ---- Internal ----

(define-private (is-authorized (caller principal))
  (default-to false (map-get? authorized-minters caller)))

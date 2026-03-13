;; ClariFi Protocol Configuration & Governance
;; On-chain governance for protocol parameter changes and upgrades.
;; Token holders can submit proposals, vote, and execute passed proposals.

(define-constant ERR-NOT-AUTHORIZED (err u5000))
(define-constant ERR-ALREADY-EXECUTED (err u5001))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u5002))
(define-constant ERR-VOTING-CLOSED (err u5003))
(define-constant ERR-VOTING-OPEN (err u5004))
(define-constant ERR-ALREADY-VOTED (err u5005))
(define-constant ERR-INVALID-PARAMS (err u5006))

;; Governance parameters
(define-data-var protocol-owner principal tx-sender)
(define-data-var proposal-counter uint u0)
(define-data-var voting-period uint u144)    ;; ~1 day in Stacks blocks
(define-data-var quorum-threshold uint u1000000) ;; minimum total votes to pass
(define-data-var proposal-deposit uint u100000000) ;; 100 CLFI to submit a proposal

;; ---- Proposal Storage ----

(define-map proposals uint {
  proposer: principal,
  title: (string-ascii 64),
  description: (string-ascii 256),
  start-block: uint,
  end-block: uint,
  yes-votes: uint,
  no-votes: uint,
  executed: bool,
  action-id: uint
})

;; Track individual votes to prevent double voting
(define-map vote-records { proposal-id: uint, voter: principal } { amount: uint, support: bool })

;; ---- Read-Only Functions ----

(define-read-only (get-proposal (id uint))
  (map-get? proposals id))

(define-read-only (get-proposal-count)
  (ok (var-get proposal-counter)))

(define-read-only (get-vote (id uint) (voter principal))
  (map-get? vote-records { proposal-id: id, voter: voter }))

(define-read-only (get-owner)
  (ok (var-get protocol-owner)))

(define-read-only (get-governance-params)
  (ok {
    voting-period: (var-get voting-period),
    quorum: (var-get quorum-threshold),
    deposit: (var-get proposal-deposit)
  }))

;; ---- Proposal Lifecycle ----

;; Submit a new governance proposal
(define-public (submit-proposal (title (string-ascii 64)) (description (string-ascii 256)) (action-id uint))
  (let (
    (id (var-get proposal-counter))
    (proposer tx-sender)
    (deposit (var-get proposal-deposit))
  )
    ;; Lock proposal deposit
    (try! (contract-call? .clarifi-token transfer deposit proposer (as-contract tx-sender) none))
    (map-set proposals id {
      proposer: proposer,
      title: title,
      description: description,
      start-block: block-height,
      end-block: (+ block-height (var-get voting-period)),
      yes-votes: u0,
      no-votes: u0,
      executed: false,
      action-id: action-id
    })
    (var-set proposal-counter (+ id u1))
    (print { event: "proposal-created", id: id, proposer: proposer, title: title })
    (ok id)))

;; Cast a vote on an active proposal
(define-public (vote (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
    (voter tx-sender)
    (voting-power (unwrap-panic (contract-call? .clarifi-token get-balance voter)))
  )
    ;; Ensure voting is still open
    (asserts! (>= block-height (get start-block proposal)) ERR-VOTING-CLOSED)
    (asserts! (<= block-height (get end-block proposal)) ERR-VOTING-CLOSED)
    ;; Prevent double voting
    (asserts! (is-none (map-get? vote-records { proposal-id: proposal-id, voter: voter }))
      ERR-ALREADY-VOTED)
    ;; Record the vote
    (map-set vote-records { proposal-id: proposal-id, voter: voter }
      { amount: voting-power, support: support })
    ;; Update vote tallies
    (map-set proposals proposal-id
      (merge proposal
        (if support
          { yes-votes: (+ (get yes-votes proposal) voting-power), no-votes: (get no-votes proposal) }
          { yes-votes: (get yes-votes proposal), no-votes: (+ (get no-votes proposal) voting-power) })))
    (print { event: "vote-cast", proposal: proposal-id, voter: voter, support: support, weight: voting-power })
    (ok true)))

;; Execute a proposal that has passed the voting period
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
  )
    ;; Voting period must be over
    (asserts! (> block-height (get end-block proposal)) ERR-VOTING-OPEN)
    ;; Must not have been executed already
    (asserts! (not (get executed proposal)) ERR-ALREADY-EXECUTED)
    ;; Must meet quorum
    (asserts! (>= (+ (get yes-votes proposal) (get no-votes proposal)) (var-get quorum-threshold))
      ERR-NOT-AUTHORIZED)
    ;; Mark as executed
    (map-set proposals proposal-id (merge proposal { executed: true }))
    ;; Return deposit to proposer
    (try! (as-contract (contract-call? .clarifi-token transfer
      (var-get proposal-deposit) tx-sender (get proposer proposal) none)))
    (print { event: "proposal-executed", id: proposal-id, action: (get action-id proposal) })
    (ok (get action-id proposal))))

;; ---- Owner Functions ----

;; Transfer protocol ownership to a new address
(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-owner)) ERR-NOT-AUTHORIZED)
    (var-set protocol-owner new-owner)
    (print { event: "ownership-transferred", from: tx-sender, to: new-owner })
    (ok true)))

;; Emergency: pause the lending pool immediately
(define-public (emergency-pause)
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-owner)) ERR-NOT-AUTHORIZED)
    (try! (contract-call? .lending-pool set-pool-paused true))
    (print { event: "emergency-pause", caller: tx-sender })
    (ok true)))

;; Emergency: unpause the lending pool
(define-public (emergency-unpause)
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-owner)) ERR-NOT-AUTHORIZED)
    (try! (contract-call? .lending-pool set-pool-paused false))
    (ok true)))

;; Update governance parameters
(define-public (set-voting-period (blocks uint))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (> blocks u0) ERR-INVALID-PARAMS)
    (ok (var-set voting-period blocks))))

(define-public (set-quorum (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-PARAMS)
    (ok (var-set quorum-threshold amount))))

(define-public (set-proposal-deposit (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get protocol-owner)) ERR-NOT-AUTHORIZED)
    (ok (var-set proposal-deposit amount))))

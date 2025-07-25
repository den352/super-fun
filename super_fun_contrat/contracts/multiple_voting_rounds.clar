;; Advanced Multi-Round Voting Smart Contract
;; Enhanced with delegation, weighted voting, proposals, and advanced features

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ROUND-NOT-FOUND (err u101))
(define-constant ERR-ROUND-NOT-ACTIVE (err u102))
(define-constant ERR-CANDIDATE-NOT-FOUND (err u103))
(define-constant ERR-ALREADY-VOTED (err u104))
(define-constant ERR-INVALID-ROUND (err u105))
(define-constant ERR-INSUFFICIENT-TOKENS (err u106))
(define-constant ERR-DELEGATION-EXISTS (err u107))
(define-constant ERR-CANNOT-DELEGATE-TO-SELF (err u108))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u109))
(define-constant ERR-VOTING-PERIOD-ENDED (err u110))
(define-constant ERR-MINIMUM-PARTICIPATION-NOT-MET (err u111))
(define-constant ERR-INVALID-QUORUM (err u112))
(define-constant ERR-ROUND-ALREADY-FINALIZED (err u113))
(define-constant ERR-NOT-ELIGIBLE (err u114))
(define-constant ERR-EMERGENCY-PAUSE (err u115))

;; Contract configuration
(define-data-var contract-owner principal tx-sender)
(define-data-var current-round-id uint u0)
(define-data-var current-proposal-id uint u0)
(define-data-var emergency-pause bool false)
(define-data-var governance-token principal 'SP000000000000000000002Q6VF78) ;; Placeholder

;; Round types
(define-constant ROUND-TYPE-SIMPLE u1)
(define-constant ROUND-TYPE-WEIGHTED u2)
(define-constant ROUND-TYPE-DELEGATED u3)
(define-constant ROUND-TYPE-PROPOSAL u4)

;; Vote privacy levels
(define-constant PRIVACY-PUBLIC u1)
(define-constant PRIVACY-ANONYMOUS u2)
(define-constant PRIVACY-HIDDEN u3)

;; Round data structure
(define-map rounds
  { round-id: uint }
  {
    status: uint,
    round-type: uint,
    name: (string-ascii 50),
    description: (string-ascii 500),
    start-block: uint,
    end-block: uint,
    min-participation: uint,
    quorum-threshold: uint,
    privacy-level: uint,
    total-votes-cast: uint,
    is-finalized: bool,
    winner-candidate-id: (optional uint),
    creator: principal
  }
)

;; Enhanced candidate structure
(define-map round-candidates
  { round-id: uint, candidate-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    proposal-details: (optional (string-ascii 1000)),
    supporter: principal,
    is-active: bool
  }
)

;; Weighted vote tracking
(define-map candidate-votes
  { round-id: uint, candidate-id: uint }
  { 
    vote-count: uint,
    weighted-votes: uint,
    delegated-votes: uint
  }
)

;; Enhanced voter history with weight tracking
(define-map voter-round-history
  { voter: principal, round-id: uint }
  { 
    has-voted: bool, 
    candidate-id: uint,
    vote-weight: uint,
    timestamp: uint,
    is-delegated: bool
  }
)

;; Vote delegation system
(define-map vote-delegations
  { delegator: principal, round-id: uint }
  {
    delegate: principal,
    weight: uint,
    is-active: bool
  }
)

;; Track received delegations
(define-map delegate-power
  { delegate: principal, round-id: uint }
  { total-delegated-weight: uint }
)

;; Voter eligibility and weights
(define-map voter-eligibility
  { voter: principal }
  { 
    is-eligible: bool,
    vote-weight: uint,
    reputation-score: uint,
    registration-block: uint
  }
)

;; Proposal system for governance votes
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 1000),
    proposer: principal,
    round-id: uint,
    proposal-type: uint,
    target-contract: (optional principal),
    function-name: (optional (string-ascii 50)),
    parameters: (optional (string-ascii 500))
  }
)

;; Round candidate count tracking
(define-map round-candidate-count
  { round-id: uint }
  { count: uint }
)

;; Round statistics
(define-map round-stats
  { round-id: uint }
  {
    total-eligible-voters: uint,
    participation-rate: uint,
    total-weight-cast: uint,
    total-votes-cast: uint,
    delegation-rate: uint
  }
)

;; Access control roles
(define-map user-roles
  { user: principal }
  { 
    is-admin: bool,
    is-moderator: bool,
    can-create-rounds: bool,
    can-propose: bool
  }
)

;; Event logging for transparency
(define-map event-log
  { event-id: uint }
  {
    event-type: (string-ascii 50),
    actor: principal,
    round-id: (optional uint),
    timestamp: uint,
    details: (string-ascii 200)
  }
)

(define-data-var next-event-id uint u1)

;; Basic read-only functions (defined first for use by utility functions)

(define-read-only (get-round-info (round-id uint))
  (map-get? rounds { round-id: round-id })
)

;; Utility functions (defined first to be used by other functions)

(define-private (is-admin (user principal))
  (default-to false
    (get is-admin
      (map-get? user-roles { user: user })
    )
  )
)

(define-private (is-moderator (user principal))
  (or
    (is-eq user (var-get contract-owner))
    (default-to false
      (get is-moderator
        (map-get? user-roles { user: user })
      )
    )
  )
)

(define-private (can-create-rounds (user principal))
  (or
    (is-moderator user)
    (default-to false
      (get can-create-rounds
        (map-get? user-roles { user: user })
      )
    )
  )
)

(define-private (can-propose (user principal))
  (or
    (is-moderator user)
    (default-to false
      (get can-propose
        (map-get? user-roles { user: user })
      )
    )
  )
)

(define-private (is-eligible (voter principal))
  (default-to false
    (get is-eligible
      (map-get? voter-eligibility { voter: voter })
    )
  )
)

(define-private (get-voter-weight (voter principal))
  (default-to u1
    (get vote-weight
      (map-get? voter-eligibility { voter: voter })
    )
  )
)

(define-private (update-round-stats (round-id uint))
  (let (
    (current-stats (default-to 
                     { total-eligible-voters: u0, participation-rate: u0, total-weight-cast: u0, total-votes-cast: u0, delegation-rate: u0 }
                     (map-get? round-stats { round-id: round-id })))
    (round-info (unwrap-panic (get-round-info round-id)))
  )
    (map-set round-stats
      { round-id: round-id }
      (merge current-stats {
        total-votes-cast: (+ (get total-votes-cast current-stats) u1)
      })
    )
  )
)

(define-private (find-round-winner (round-id uint))
  ;; Simplified winner finding - returns candidate with most weighted votes
  ;; In production, this would need more sophisticated logic
  (some u1) ;; Placeholder - would implement actual winner calculation
)

(define-private (log-event (event-type (string-ascii 50)) (details (string-ascii 200)))
  (let ((event-id (var-get next-event-id)))
    (map-set event-log
      { event-id: event-id }
      {
        event-type: event-type,
        actor: tx-sender,
        round-id: none,
        timestamp: block-height,
        details: details
      }
    )
    (var-set next-event-id (+ event-id u1))
  )
)

;; Additional read-only functions

(define-read-only (get-candidate (round-id uint) (candidate-id uint))
  (map-get? round-candidates { round-id: round-id, candidate-id: candidate-id })
)

(define-read-only (get-candidate-votes (round-id uint) (candidate-id uint))
  (map-get? candidate-votes { round-id: round-id, candidate-id: candidate-id })
)

(define-read-only (get-voter-eligibility (voter principal))
  (map-get? voter-eligibility { voter: voter })
)

(define-read-only (get-delegation (delegator principal) (round-id uint))
  (map-get? vote-delegations { delegator: delegator, round-id: round-id })
)

(define-read-only (get-delegate-power (delegate principal) (round-id uint))
  (default-to u0
    (get total-delegated-weight
      (map-get? delegate-power { delegate: delegate, round-id: round-id })
    )
  )
)

(define-read-only (get-voting-power (voter principal) (round-id uint))
  (let (
    (base-weight (default-to u1 
                   (get vote-weight 
                     (map-get? voter-eligibility { voter: voter }))))
    (delegated-power (get-delegate-power voter round-id))
  )
    (+ base-weight delegated-power)
  )
)

(define-read-only (has-voted (voter principal) (round-id uint))
  (default-to false
    (get has-voted
      (map-get? voter-round-history { voter: voter, round-id: round-id })
    )
  )
)

(define-read-only (get-candidate-count (round-id uint))
  (default-to u0
    (get count
      (map-get? round-candidate-count { round-id: round-id })
    )
  )
)

(define-read-only (get-round-results (round-id uint))
  (let (
    (round-info (map-get? rounds { round-id: round-id }))
    (stats (map-get? round-stats { round-id: round-id }))
  )
    {
      round-info: round-info,
      stats: stats,
      candidate-count: (get-candidate-count round-id)
    }
  )
)

(define-read-only (is-round-active (round-id uint))
  (and
    (not (var-get emergency-pause))
    (match (get-round-info round-id)
      round-info (and 
                   (is-eq (get status round-info) u1)
                   (>= block-height (get start-block round-info))
                   (<= block-height (get end-block round-info))
                   (not (get is-finalized round-info)))
      false)
  )
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (calculate-participation-rate (round-id uint))
  (match (map-get? round-stats { round-id: round-id })
    stats (if (> (get total-eligible-voters stats) u0)
            (/ (* (get total-weight-cast stats) u100) (get total-eligible-voters stats))
            u0)
    u0
  )
)

;; Administrative functions

(define-public (set-governance-token (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set governance-token token-contract)
    (ok true)
  )
)

(define-public (set-emergency-pause (pause bool))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (var-set emergency-pause pause)
    (log-event "EMERGENCY_PAUSE" (if pause "ACTIVATED" "DEACTIVATED"))
    (ok true)
  )
)

(define-public (set-user-role (user principal) (admin-role bool) (moderator-role bool) (creator-role bool) (proposer-role bool))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (map-set user-roles
      { user: user }
      {
        is-admin: admin-role,
        is-moderator: moderator-role,
        can-create-rounds: creator-role,
        can-propose: proposer-role
      }
    )
    (ok true)
  )
)

(define-public (register-voter (voter principal) (vote-weight uint) (reputation-score uint))
  (begin
    (asserts! (is-admin tx-sender) ERR-NOT-AUTHORIZED)
    (map-set voter-eligibility
      { voter: voter }
      {
        is-eligible: true,
        vote-weight: vote-weight,
        reputation-score: reputation-score,
        registration-block: block-height
      }
    )
    (log-event "VOTER_REGISTERED" "")
    (ok true)
  )
)

(define-public (create-round 
  (round-type uint) 
  (name (string-ascii 50)) 
  (description (string-ascii 500))
  (start-block uint) 
  (end-block uint)
  (min-participation uint)
  (quorum-threshold uint)
  (privacy-level uint))
  (let ((new-round-id (+ (var-get current-round-id) u1)))
    (asserts! (not (var-get emergency-pause)) ERR-EMERGENCY-PAUSE)
    (asserts! (can-create-rounds tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> end-block start-block) ERR-INVALID-ROUND)
    (asserts! (<= quorum-threshold u100) ERR-INVALID-QUORUM)
    
    (map-set rounds
      { round-id: new-round-id }
      {
        status: u0,
        round-type: round-type,
        name: name,
        description: description,
        start-block: start-block,
        end-block: end-block,
        min-participation: min-participation,
        quorum-threshold: quorum-threshold,
        privacy-level: privacy-level,
        total-votes-cast: u0,
        is-finalized: false,
        winner-candidate-id: none,
        creator: tx-sender
      }
    )
    
    (map-set round-candidate-count
      { round-id: new-round-id }
      { count: u0 }
    )
    
    (var-set current-round-id new-round-id)
    (log-event "ROUND_CREATED" "")
    (ok new-round-id)
  )
)

(define-public (add-candidate (round-id uint) (name (string-ascii 50)) (description (string-ascii 200)) (proposal-details (optional (string-ascii 1000))))
  (let (
    (current-count (get-candidate-count round-id))
    (new-candidate-id (+ current-count u1))
  )
    (asserts! (is-moderator tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (get-round-info round-id)) ERR-ROUND-NOT-FOUND)
    
    (map-set round-candidates
      { round-id: round-id, candidate-id: new-candidate-id }
      {
        name: name,
        description: description,
        proposal-details: proposal-details,
        supporter: tx-sender,
        is-active: true
      }
    )
    
    (map-set candidate-votes
      { round-id: round-id, candidate-id: new-candidate-id }
      { vote-count: u0, weighted-votes: u0, delegated-votes: u0 }
    )
    
    (map-set round-candidate-count
      { round-id: round-id }
      { count: new-candidate-id }
    )
    
    (log-event "CANDIDATE_ADDED" "")
    (ok new-candidate-id)
  )
)

(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 1000))
  (round-id uint)
  (proposal-type uint)
  (target-contract (optional principal))
  (function-name (optional (string-ascii 50)))
  (parameters (optional (string-ascii 500))))
  (let ((new-proposal-id (+ (var-get current-proposal-id) u1)))
    (asserts! (can-propose tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (get-round-info round-id)) ERR-ROUND-NOT-FOUND)
    
    (map-set proposals
      { proposal-id: new-proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        round-id: round-id,
        proposal-type: proposal-type,
        target-contract: target-contract,
        function-name: function-name,
        parameters: parameters
      }
    )
    
    (var-set current-proposal-id new-proposal-id)
    (log-event "PROPOSAL_CREATED" "")
    (ok new-proposal-id)
  )
)

;; Delegation functions

(define-public (delegate-vote (delegate principal) (round-id uint))
  (let (
    (delegator-weight (get-voter-weight tx-sender))
  )
    (asserts! (not (is-eq tx-sender delegate)) ERR-CANNOT-DELEGATE-TO-SELF)
    (asserts! (is-round-active round-id) ERR-ROUND-NOT-ACTIVE)
    (asserts! (is-eligible tx-sender) ERR-NOT-ELIGIBLE)
    (asserts! (is-none (get-delegation tx-sender round-id)) ERR-DELEGATION-EXISTS)
    
    (map-set vote-delegations
      { delegator: tx-sender, round-id: round-id }
      {
        delegate: delegate,
        weight: delegator-weight,
        is-active: true
      }
    )
    
    (let ((current-power (get-delegate-power delegate round-id)))
      (map-set delegate-power
        { delegate: delegate, round-id: round-id }
        { total-delegated-weight: (+ current-power delegator-weight) }
      )
    )
    
    (log-event "VOTE_DELEGATED" "")
    (ok true)
  )
)

(define-public (revoke-delegation (round-id uint))
  (match (get-delegation tx-sender round-id)
    delegation 
    (let (
      (delegate (get delegate delegation))
      (weight (get weight delegation))
      (current-power (get-delegate-power delegate round-id))
    )
      (map-delete vote-delegations { delegator: tx-sender, round-id: round-id })
      (map-set delegate-power
        { delegate: delegate, round-id: round-id }
        { total-delegated-weight: (- current-power weight) }
      )
      (log-event "DELEGATION_REVOKED" "")
      (ok true)
    )
    ERR-DELEGATION-EXISTS
  )
)

;; Enhanced voting functions

(define-public (vote (round-id uint) (candidate-id uint))
  (let (
    (voter-power (get-voting-power tx-sender round-id))
    (current-votes (default-to 
                     { vote-count: u0, weighted-votes: u0, delegated-votes: u0 }
                     (get-candidate-votes round-id candidate-id)))
  )
    (asserts! (is-round-active round-id) ERR-ROUND-NOT-ACTIVE)
    (asserts! (is-eligible tx-sender) ERR-NOT-ELIGIBLE)
    (asserts! (is-some (get-candidate round-id candidate-id)) ERR-CANDIDATE-NOT-FOUND)
    (asserts! (not (has-voted tx-sender round-id)) ERR-ALREADY-VOTED)
    
    ;; Record the vote
    (map-set voter-round-history
      { voter: tx-sender, round-id: round-id }
      { 
        has-voted: true, 
        candidate-id: candidate-id,
        vote-weight: voter-power,
        timestamp: block-height,
        is-delegated: false
      }
    )
    
    ;; Update vote counts
    (map-set candidate-votes
      { round-id: round-id, candidate-id: candidate-id }
      { 
        vote-count: (+ (get vote-count current-votes) u1),
        weighted-votes: (+ (get weighted-votes current-votes) voter-power),
        delegated-votes: (get delegated-votes current-votes)
      }
    )
    
    ;; Update round statistics
    (update-round-stats round-id)
    (log-event "VOTE_CAST" "")
    (ok true)
  )
)

(define-public (vote-with-delegation (round-id uint) (candidate-id uint))
  (let (
    (base-power (get-voter-weight tx-sender))
    (delegated-power (get-delegate-power tx-sender round-id))
    (total-power (+ base-power delegated-power))
    (current-votes (default-to 
                     { vote-count: u0, weighted-votes: u0, delegated-votes: u0 }
                     (get-candidate-votes round-id candidate-id)))
  )
    (asserts! (is-round-active round-id) ERR-ROUND-NOT-ACTIVE)
    (asserts! (is-eligible tx-sender) ERR-NOT-ELIGIBLE)
    (asserts! (is-some (get-candidate round-id candidate-id)) ERR-CANDIDATE-NOT-FOUND)
    (asserts! (not (has-voted tx-sender round-id)) ERR-ALREADY-VOTED)
    
    ;; Record the vote with delegation
    (map-set voter-round-history
      { voter: tx-sender, round-id: round-id }
      { 
        has-voted: true, 
        candidate-id: candidate-id,
        vote-weight: total-power,
        timestamp: block-height,
        is-delegated: (> delegated-power u0)
      }
    )
    
    ;; Update vote counts
    (map-set candidate-votes
      { round-id: round-id, candidate-id: candidate-id }
      { 
        vote-count: (+ (get vote-count current-votes) u1),
        weighted-votes: (+ (get weighted-votes current-votes) total-power),
        delegated-votes: (+ (get delegated-votes current-votes) delegated-power)
      }
    )
    
    (update-round-stats round-id)
    (log-event "DELEGATED_VOTE_CAST" "")
    (ok true)
  )
)

(define-public (finalize-round (round-id uint))
  (let (
    (round-info (unwrap! (get-round-info round-id) ERR-ROUND-NOT-FOUND))
    (participation-rate (calculate-participation-rate round-id))
  )
    (asserts! (is-moderator tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> block-height (get end-block round-info)) ERR-ROUND-NOT-ACTIVE)
    (asserts! (not (get is-finalized round-info)) ERR-ROUND-ALREADY-FINALIZED)
    (asserts! (>= participation-rate (get min-participation round-info)) ERR-MINIMUM-PARTICIPATION-NOT-MET)
    
    ;; Find winner (simplified - could be enhanced with tie-breaking)
    (let ((winner-id (find-round-winner round-id)))
      (map-set rounds
        { round-id: round-id }
        (merge round-info {
          status: u2,
          is-finalized: true,
          winner-candidate-id: winner-id
        })
      )
      
      (log-event "ROUND_FINALIZED" "")
      (ok winner-id)
    )
  )
)





;; Governance functions

(define-public (activate-round (round-id uint))
  (begin
    (asserts! (is-moderator tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-some (get-round-info round-id)) ERR-ROUND-NOT-FOUND)
    
    (map-set rounds
      { round-id: round-id }
      (merge 
        (unwrap! (get-round-info round-id) ERR-ROUND-NOT-FOUND)
        { status: u1 }
      )
    )
    (log-event "ROUND_ACTIVATED" "")
    (ok true)
  )
)

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (var-set contract-owner new-owner)
    (log-event "OWNERSHIP_TRANSFERRED" "")
    (ok true)
  )
)
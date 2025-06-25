;; Senior Housing Cooperative Contract
;; Manages cooperative membership, shared resources, and governance

;; Constants
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_AMOUNT (err u400))
(define-constant ERR_INSUFFICIENT_FUNDS (err u402))
(define-constant ERR_INVALID_STATUS (err u403))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Data structures
(define-map Members
  { member-id: principal }
  {
    name: (string-utf8 100),
    unit-number: (string-utf8 20),
    join-date: uint,
    monthly-fee: uint,
    status: (string-ascii 20),
    emergency-contact: (string-utf8 200),
    care-level: (string-ascii 20)
  }
)

(define-map SharedResources
  { resource-id: uint }
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    capacity: uint,
    hourly-rate: uint,
    available: bool,
    maintenance-due: uint
  }
)

(define-map Activities
  { activity-id: uint }
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    organizer: principal,
    scheduled-time: uint,
    location: (string-utf8 100),
    max-participants: uint,
    current-participants: uint,
    category: (string-ascii 50),
    status: (string-ascii 20)
  }
)

(define-map ActivityParticipants
  { activity-id: uint, participant: principal }
  { enrolled-at: uint, status: (string-ascii 20) }
)

(define-map ResourceBookings
  { booking-id: uint }
  {
    resource-id: uint,
    member: principal,
    start-time: uint,
    end-time: uint,
    total-cost: uint,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map FamilyAccess
  { member: principal, family-member: principal }
  {
    relationship: (string-utf8 50),
    access-level: (string-ascii 20),
    granted-at: uint,
    last-access: uint
  }
)

(define-map GovernanceProposals
  { proposal-id: uint }
  {
    title: (string-utf8 200),
    description: (string-utf8 1000),
    proposer: principal,
    created-at: uint,
    voting-ends: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20),
    proposal-type: (string-ascii 50)
  }
)

(define-map ProposalVotes
  { proposal-id: uint, voter: principal }
  { vote: bool, voted-at: uint }
)

;; Data variables
(define-data-var next-resource-id uint u1)
(define-data-var next-activity-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var cooperative-fund uint u0)
(define-data-var monthly-base-fee uint u500000000) ;; 500 STX in microSTX
(define-data-var member-count uint u0)

;; Member management functions
(define-public (register-member
  (member principal)
  (name (string-utf8 100))
  (unit-number (string-utf8 20))
  (emergency-contact (string-utf8 200))
  (care-level (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? Members { member-id: member })) ERR_ALREADY_EXISTS)
    (map-set Members
      { member-id: member }
      {
        name: name,
        unit-number: unit-number,
        join-date: stacks-block-height,
        monthly-fee: (var-get monthly-base-fee),
        status: "active",
        emergency-contact: emergency-contact,
        care-level: care-level
      })
    (var-set member-count (+ (var-get member-count) u1))
    (ok true)))

(define-public (update-member-info
  (member principal)
  (emergency-contact (string-utf8 200))
  (care-level (string-ascii 20)))
  (let ((member-data (unwrap! (map-get? Members { member-id: member }) ERR_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender member) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (map-set Members
      { member-id: member }
      (merge member-data {
        emergency-contact: emergency-contact,
        care-level: care-level
      }))
    (ok true)))

(define-public (pay-monthly-fee (member principal))
  (let ((member-data (unwrap! (map-get? Members { member-id: member }) ERR_NOT_FOUND)))
    (asserts! (is-eq tx-sender member) ERR_UNAUTHORIZED)
    (try! (stx-transfer? (get monthly-fee member-data) tx-sender (as-contract tx-sender)))
    (var-set cooperative-fund (+ (var-get cooperative-fund) (get monthly-fee member-data)))
    (ok true)))

;; Shared resource management
(define-public (add-shared-resource
  (name (string-utf8 100))
  (description (string-utf8 500))
  (capacity uint)
  (hourly-rate uint))
  (let ((resource-id (var-get next-resource-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set SharedResources
      { resource-id: resource-id }
      {
        name: name,
        description: description,
        capacity: capacity,
        hourly-rate: hourly-rate,
        available: true,
        maintenance-due: (+ stacks-block-height u8760) ;; ~6 months
      })
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)))

(define-public (book-resource
  (resource-id uint)
  (start-time uint)
  (duration-hours uint))
  (let (
    (resource (unwrap! (map-get? SharedResources { resource-id: resource-id }) ERR_NOT_FOUND))
    (booking-id (var-get next-booking-id))
    (total-cost (* (get hourly-rate resource) duration-hours))
    (end-time (+ start-time (* duration-hours u144))) ;; ~1 hour in blocks
  )
    (asserts! (is-some (map-get? Members { member-id: tx-sender })) ERR_UNAUTHORIZED)
    (asserts! (get available resource) ERR_INVALID_STATUS)
    (asserts! (>= start-time stacks-block-height) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    (map-set ResourceBookings
      { booking-id: booking-id }
      {
        resource-id: resource-id,
        member: tx-sender,
        start-time: start-time,
        end-time: end-time,
        total-cost: total-cost,
        status: "confirmed",
        created-at: stacks-block-height
      })
    (var-set next-booking-id (+ booking-id u1))
    (var-set cooperative-fund (+ (var-get cooperative-fund) total-cost))
    (ok booking-id)))

;; Activity management
(define-public (create-activity
  (name (string-utf8 100))
  (description (string-utf8 500))
  (scheduled-time uint)
  (location (string-utf8 100))
  (max-participants uint)
  (category (string-ascii 50)))
  (let ((activity-id (var-get next-activity-id)))
    (asserts! (is-some (map-get? Members { member-id: tx-sender })) ERR_UNAUTHORIZED)
    (asserts! (> scheduled-time stacks-block-height) ERR_INVALID_AMOUNT)
    (map-set Activities
      { activity-id: activity-id }
      {
        name: name,
        description: description,
        organizer: tx-sender,
        scheduled-time: scheduled-time,
        location: location,
        max-participants: max-participants,
        current-participants: u0,
        category: category,
        status: "scheduled"
      })
    (var-set next-activity-id (+ activity-id u1))
    (ok activity-id)))

(define-public (join-activity (activity-id uint))
  (let ((activity (unwrap! (map-get? Activities { activity-id: activity-id }) ERR_NOT_FOUND)))
    (asserts! (is-some (map-get? Members { member-id: tx-sender })) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? ActivityParticipants { activity-id: activity-id, participant: tx-sender })) ERR_ALREADY_EXISTS)
    (asserts! (< (get current-participants activity) (get max-participants activity)) ERR_INVALID_AMOUNT)
    (asserts! (is-eq (get status activity) "scheduled") ERR_INVALID_STATUS)
    (map-set ActivityParticipants
      { activity-id: activity-id, participant: tx-sender }
      { enrolled-at: stacks-block-height, status: "enrolled" })
    (map-set Activities
      { activity-id: activity-id }
      (merge activity { current-participants: (+ (get current-participants activity) u1) }))
    (ok true)))

;; Family communication portal
(define-public (grant-family-access
  (family-member principal)
  (relationship (string-utf8 50))
  (access-level (string-ascii 20)))
  (begin
    (asserts! (is-some (map-get? Members { member-id: tx-sender })) ERR_UNAUTHORIZED)
    (map-set FamilyAccess
      { member: tx-sender, family-member: family-member }
      {
        relationship: relationship,
        access-level: access-level,
        granted-at: stacks-block-height,
        last-access: u0
      })
    (ok true)))

(define-public (revoke-family-access (family-member principal))
  (begin
    (asserts! (is-some (map-get? Members { member-id: tx-sender })) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? FamilyAccess { member: tx-sender, family-member: family-member })) ERR_NOT_FOUND)
    (map-delete FamilyAccess { member: tx-sender, family-member: family-member })
    (ok true)))

;; Governance functions
(define-public (create-proposal
  (title (string-utf8 200))
  (description (string-utf8 1000))
  (proposal-type (string-ascii 50))
  (voting-duration uint))
  (let ((proposal-id (var-get next-proposal-id)))
    (asserts! (is-some (map-get? Members { member-id: tx-sender })) ERR_UNAUTHORIZED)
    (asserts! (> voting-duration u0) ERR_INVALID_AMOUNT)
    (map-set GovernanceProposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        created-at: stacks-block-height,
        voting-ends: (+ stacks-block-height voting-duration),
        votes-for: u0,
        votes-against: u0,
        status: "active",
        proposal-type: proposal-type
      })
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let ((proposal (unwrap! (map-get? GovernanceProposals { proposal-id: proposal-id }) ERR_NOT_FOUND)))
    (asserts! (is-some (map-get? Members { member-id: tx-sender })) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? ProposalVotes { proposal-id: proposal-id, voter: tx-sender })) ERR_ALREADY_EXISTS)
    (asserts! (< stacks-block-height (get voting-ends proposal)) ERR_INVALID_STATUS)
    (asserts! (is-eq (get status proposal) "active") ERR_INVALID_STATUS)
    (map-set ProposalVotes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for, voted-at: stacks-block-height })
    (if vote-for
      (map-set GovernanceProposals
        { proposal-id: proposal-id }
        (merge proposal { votes-for: (+ (get votes-for proposal) u1) }))
      (map-set GovernanceProposals
        { proposal-id: proposal-id }
        (merge proposal { votes-against: (+ (get votes-against proposal) u1) })))
    (ok true)))

;; Read-only functions
(define-read-only (get-member-info (member principal))
  (map-get? Members { member-id: member }))

(define-read-only (get-resource-info (resource-id uint))
  (map-get? SharedResources { resource-id: resource-id }))

(define-read-only (get-activity-info (activity-id uint))
  (map-get? Activities { activity-id: activity-id }))

(define-read-only (get-booking-info (booking-id uint))
  (map-get? ResourceBookings { booking-id: booking-id }))

(define-read-only (get-family-access (member principal) (family-member principal))
  (map-get? FamilyAccess { member: member, family-member: family-member }))

(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? GovernanceProposals { proposal-id: proposal-id }))

(define-read-only (get-cooperative-stats)
  {
    member-count: (var-get member-count),
    cooperative-fund: (var-get cooperative-fund),
    monthly-base-fee: (var-get monthly-base-fee),
    total-resources: (- (var-get next-resource-id) u1),
    total-activities: (- (var-get next-activity-id) u1),
    total-proposals: (- (var-get next-proposal-id) u1)
  })

(define-read-only (is-member (user principal))
  (is-some (map-get? Members { member-id: user })))

(define-read-only (has-family-access (member principal) (family-member principal))
  (is-some (map-get? FamilyAccess { member: member, family-member: family-member })))

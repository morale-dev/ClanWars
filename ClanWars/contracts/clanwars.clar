;; ClanWars - On-chain League System for Gaming Clans
;; Teams register, challenge each other, and climb ranks with automated rewards

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-CLAN-NOT-FOUND (err u404))
(define-constant ERR-CHALLENGE-NOT-FOUND (err u405))
(define-constant ERR-INSUFFICIENT-FUNDS (err u402))
(define-constant ERR-INVALID-STATUS (err u403))
(define-constant ERR-CLAN-EXISTS (err u406))
(define-constant ERR-ALREADY-MEMBER (err u407))
(define-constant ERR-CLAN-FULL (err u408))
(define-constant ERR-NOT-MEMBER (err u409))

;; Data Variables
(define-data-var next-clan-id uint u1)
(define-data-var next-challenge-id uint u1)
(define-data-var season-number uint u1)
(define-data-var registration-fee uint u1000000) ;; 1 STX

;; Data Maps
(define-map clans
  uint
  {
    name: (string-ascii 50),
    leader: principal,
    members: (list 10 principal),
    wins: uint,
    losses: uint,
    rank-points: uint,
    active: bool
  })

(define-map clan-names
  (string-ascii 50)
  uint)

(define-map challenges
  uint
  {
    challenger: uint,
    challenged: uint,
    stake: uint,
    status: (string-ascii 20),
    winner: (optional uint),
    created-at: uint
  })

(define-map member-clans
  principal
  uint)

(define-map season-rewards
  {season: uint, clan-id: uint}
  uint)

  ;; Register a new clan
(define-public (register-clan (name (string-ascii 50)) (members (list 10 principal)))
  (let ((clan-id (var-get next-clan-id)))
    (asserts! (is-none (map-get? clan-names name)) ERR-CLAN-EXISTS)
    (try! (stx-transfer? (var-get registration-fee) tx-sender (as-contract tx-sender)))
    
    (map-set clans clan-id {
      name: name,
      leader: tx-sender,
      members: members,
      wins: u0,
      losses: u0,
      rank-points: u1000,
      active: true
    })
    
    (map-set clan-names name clan-id)
    (map-set member-clans tx-sender clan-id)
    
    ;; Map all members to clan
    (fold set-member-to-clan members clan-id)
    
    (var-set next-clan-id (+ clan-id u1))
    (ok clan-id)))

;; Add member to clan
(define-public (add-member (clan-id uint) (new-member principal))
  (let (
    (clan (unwrap! (map-get? clans clan-id) ERR-CLAN-NOT-FOUND))
    (current-members (get members clan))
  )
    (asserts! (is-eq tx-sender (get leader clan)) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? member-clans new-member)) ERR-ALREADY-MEMBER)
    (asserts! (< (len current-members) u10) ERR-CLAN-FULL)
    
    (let ((updated-members (unwrap! (as-max-len? (append current-members new-member) u10) ERR-CLAN-FULL)))
      (map-set clans clan-id (merge clan {members: updated-members}))
      (map-set member-clans new-member clan-id)
      (ok true))))

;; Remove member from clan
(define-public (remove-member (clan-id uint) (member-to-remove principal))
  (let (
    (clan (unwrap! (map-get? clans clan-id) ERR-CLAN-NOT-FOUND))
    (member-clan-id (map-get? member-clans member-to-remove))
  )
    (asserts! (is-eq tx-sender (get leader clan)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some member-clan-id) ERR-NOT-MEMBER)
    (asserts! (is-eq (unwrap-panic member-clan-id) clan-id) ERR-NOT-MEMBER)
    (asserts! (not (is-eq member-to-remove (get leader clan))) ERR-NOT-AUTHORIZED) ;; Can't remove leader
    
    (let ((updated-members (remove-member-from-list (get members clan) member-to-remove)))
      (map-set clans clan-id (merge clan {members: updated-members}))
      (map-delete member-clans member-to-remove)
      (ok true))))

;; Transfer clan leadership
(define-public (transfer-leadership (clan-id uint) (new-leader principal))
  (let (
    (clan (unwrap! (map-get? clans clan-id) ERR-CLAN-NOT-FOUND))
    (member-clan-id (map-get? member-clans new-leader))
  )
    (asserts! (is-eq tx-sender (get leader clan)) ERR-NOT-AUTHORIZED)
    (asserts! (is-some member-clan-id) ERR-NOT-MEMBER)
    (asserts! (is-eq (unwrap-panic member-clan-id) clan-id) ERR-NOT-MEMBER)
    
    (map-set clans clan-id (merge clan {leader: new-leader}))
    (ok true)))

;; Helper function for member management
(define-private (set-member-to-clan (member principal) (clan-id uint))
  (begin
    (map-set member-clans member clan-id)
    clan-id))

(define-private (remove-member-from-list (members (list 10 principal)) (target principal))
  (get filtered-list (fold build-filtered-list members {filtered-list: (list), target: target})))

(define-private (build-filtered-list (member principal) (acc {filtered-list: (list 10 principal), target: principal}))
  (let ((current-list (get filtered-list acc)))
    {
      filtered-list: (if (is-eq member (get target acc))
                        current-list  ;; Skip this member
                        (match (as-max-len? (append current-list member) u10)
                          new-list new-list
                          current-list)),  ;; If append fails, keep current list
      target: (get target acc)
    }))
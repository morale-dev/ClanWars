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

;; Create a challenge between clans
(define-public (create-challenge (challenger-id uint) (challenged-id uint) (stake uint))
  (let (
    (challenger (unwrap! (map-get? clans challenger-id) ERR-CLAN-NOT-FOUND))
    (challenged (unwrap! (map-get? clans challenged-id) ERR-CLAN-NOT-FOUND))
    (challenge-id (var-get next-challenge-id))
  )
    (asserts! (is-eq tx-sender (get leader challenger)) ERR-NOT-AUTHORIZED)
    (asserts! (and (get active challenger) (get active challenged)) ERR-INVALID-STATUS)
    (asserts! (> stake u0) ERR-INSUFFICIENT-FUNDS)
    
    (try! (stx-transfer? stake tx-sender (as-contract tx-sender)))
    
    (map-set challenges challenge-id {
      challenger: challenger-id,
      challenged: challenged-id,
      stake: stake,
      status: "pending",
      winner: none,
      created-at: burn-block-height
    })
    
    (var-set next-challenge-id (+ challenge-id u1))
    (ok challenge-id)))

;; Accept a challenge
(define-public (accept-challenge (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    (challenged-clan (unwrap! (map-get? clans (get challenged challenge)) ERR-CLAN-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender (get leader challenged-clan)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status challenge) "pending") ERR-INVALID-STATUS)
    
    (try! (stx-transfer? (get stake challenge) tx-sender (as-contract tx-sender)))
    
    (map-set challenges challenge-id (merge challenge {status: "active"}))
    (ok true)))

;; Cancel pending challenge
(define-public (cancel-challenge (challenge-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    (challenger-clan (unwrap! (map-get? clans (get challenger challenge)) ERR-CLAN-NOT-FOUND))
  )
    (asserts! (is-eq (get status challenge) "pending") ERR-INVALID-STATUS)
    (asserts! (is-eq tx-sender (get leader challenger-clan)) ERR-NOT-AUTHORIZED)
    
    ;; Refund stake to challenger
    (try! (as-contract (stx-transfer? (get stake challenge) tx-sender (get leader challenger-clan))))
    
    (map-set challenges challenge-id (merge challenge {status: "cancelled"}))
    (ok true)))

;; Report challenge result
(define-public (report-result (challenge-id uint) (winner-id uint))
  (let (
    (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    (challenger-clan (unwrap! (map-get? clans (get challenger challenge)) ERR-CLAN-NOT-FOUND))
    (challenged-clan (unwrap! (map-get? clans (get challenged challenge)) ERR-CLAN-NOT-FOUND))
  )
    (asserts! (is-eq (get status challenge) "active") ERR-INVALID-STATUS)
    (asserts! (or (is-eq tx-sender (get leader challenger-clan)) 
                  (is-eq tx-sender (get leader challenged-clan))) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq winner-id (get challenger challenge))
                  (is-eq winner-id (get challenged challenge))) ERR-NOT-AUTHORIZED)
    
    (let (
      (loser-id (if (is-eq winner-id (get challenger challenge))
                    (get challenged challenge)
                    (get challenger challenge)))
      (winner-clan (unwrap! (map-get? clans winner-id) ERR-CLAN-NOT-FOUND))
      (loser-clan (unwrap! (map-get? clans loser-id) ERR-CLAN-NOT-FOUND))
      (total-stake (* (get stake challenge) u2))
    )
      ;; Update winner stats
      (map-set clans winner-id (merge winner-clan {
        wins: (+ (get wins winner-clan) u1),
        rank-points: (+ (get rank-points winner-clan) u50)
      }))
      
      ;; Update loser stats
      (map-set clans loser-id (merge loser-clan {
        losses: (+ (get losses loser-clan) u1),
        rank-points: (if (>= (get rank-points loser-clan) u25)
                         (- (get rank-points loser-clan) u25)
                         u0)
      }))
      
      ;; Transfer winnings to winner clan leader
      (try! (as-contract (stx-transfer? total-stake tx-sender (get leader winner-clan))))
      
      ;; Update challenge
      (map-set challenges challenge-id (merge challenge {
        status: "completed",
        winner: (some winner-id)
      }))
      
      (ok true))))

;; Distribute season rewards
(define-public (distribute-season-rewards (clan-id uint))
  (let (
    (clan (unwrap! (map-get? clans clan-id) ERR-CLAN-NOT-FOUND))
    (season (var-get season-number))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? season-rewards {season: season, clan-id: clan-id})) ERR-INVALID-STATUS)
    
    (let ((reward (calculate-season-reward (get rank-points clan))))
      (try! (as-contract (stx-transfer? reward tx-sender (get leader clan))))
      (map-set season-rewards {season: season, clan-id: clan-id} reward)
      (ok reward))))

;; Start new season
(define-public (start-new-season)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set season-number (+ (var-get season-number) u1))
    (ok (var-get season-number))))

;; Helper function for reward calculation
(define-private (calculate-season-reward (rank-points uint))
  (if (>= rank-points u2000)
      u10000000  ;; 10 STX for top tier
      (if (>= rank-points u1500)
          u5000000   ;; 5 STX for high tier
          (if (>= rank-points u1000)
              u1000000   ;; 1 STX for mid tier
              u0))))     ;; No reward for low tier

;; Read-only Functions
(define-read-only (get-clan (clan-id uint))
  (map-get? clans clan-id))

(define-read-only (get-clan-by-name (name (string-ascii 50)))
  (let ((clan-id (map-get? clan-names name)))
    (match clan-id
      id (map-get? clans id)
      none)))

(define-read-only (get-challenge (challenge-id uint))
  (map-get? challenges challenge-id))

(define-read-only (get-member-clan (member principal))
  (map-get? member-clans member))

(define-read-only (get-leaderboard-position (clan-id uint))
  (let ((clan (unwrap! (map-get? clans clan-id) (err u0))))
    (ok (get rank-points clan))))

(define-read-only (get-season-reward (season uint) (clan-id uint))
  (map-get? season-rewards {season: season, clan-id: clan-id}))

(define-read-only (get-current-season)
  (var-get season-number))

(define-read-only (get-clan-stats (clan-id uint))
  (let ((clan (map-get? clans clan-id)))
    (match clan
      c (ok {
        wins: (get wins c),
        losses: (get losses c),
        rank-points: (get rank-points c),
        win-rate: (if (> (+ (get wins c) (get losses c)) u0)
                     (/ (* (get wins c) u100) (+ (get wins c) (get losses c)))
                     u0)
      })
      ERR-CLAN-NOT-FOUND)))

(define-read-only (get-pending-challenges (clan-id uint))
  (ok "Function to return pending challenges - implementation depends on indexing strategy"))

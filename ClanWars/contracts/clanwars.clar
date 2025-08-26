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
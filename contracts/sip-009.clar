
;; Copyright: (c) 2024 by Blockcity
;; This file is part of Blockcity Finance.
;; Blockcity is free software. You may redistribute or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License or
;; (at your option) any later version.
;;
;; Blockcity is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY, including without the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Blockcity. If not, see <http://www.gnu.org/licenses/>.

(impl-trait 'ST1J861WZ0AT3AFCPNBAMCTZT7KW6D5B6BS0B802H.nft-trait.nft-trait)

(define-non-fungible-token my-nft uint)

;; Storage
(define-map tokens-spender
  uint
  principal)
(define-map tokens-count
  principal
  uint)
(define-map accounts-operator
  (tuple (operator principal) (account principal))
  (tuple (is-approved bool)))

;; Internals

;; Gets the amount of tokens owned by the specified address.
(define-private (balance-of (account principal))
  (default-to u0 (map-get? tokens-count account)))

;; Gets the approved address for a token ID, or zero if no address set
(define-private (is-spender-approved (spender principal) (token-id uint))
  (let ((approved-spender
         (unwrap! (map-get? tokens-spender token-id)
                   false))) ;; return false if no specified spender
    (is-eq spender approved-spender)))

;; Tells whether an operator is approved by a given owner
(define-private (is-operator-approved (account principal) (operator principal))
  (default-to false
    (get is-approved
         (map-get? accounts-operator {operator: operator, account: account}))))

(define-private (is-owner (actor principal) (token-id uint))
  (is-eq actor
       (unwrap! (get-owner token-id) false)))

;; Returns whether the given actor can transfer a given token ID
(define-private (can-transfer (actor principal) (token-id uint))
  (or
   (is-owner actor token-id)
   (is-spender-approved actor token-id)
   (is-operator-approved (unwrap! (get-owner token-id) false) actor)))

;; Internal - Register token
(define-private (mint (new-owner principal) (token-id uint))
  (let ((current-balance (balance-of new-owner)))
      (match (nft-mint? my-nft token-id new-owner)
        success
          (begin
            (map-set tokens-count
              new-owner
              (+ u1 current-balance))
            (ok success))
        error (nft-mint-err error))))

;; Internal - Transfer token
(define-private (transfer-token (token-id uint) (owner principal) (new-owner principal))
  (let
    ((current-balance-owner (balance-of owner))
      (current-balance-new-owner (balance-of new-owner)))
    (begin
      (map-delete tokens-spender
        token-id)
      (map-set tokens-count
        owner
        (- current-balance-owner u1))
      (map-set tokens-count
        new-owner
        (+ current-balance-new-owner u1))
      (match (nft-transfer? my-nft token-id owner new-owner)
        success (ok success)
        error (nft-transfer-err error)))))

;; Public functions

;; Approves another address to transfer the given token ID
(define-public (set-spender-approval (spender principal) (token-id uint))
  (if (is-eq spender tx-sender)
      sender-equals-recipient-err
      (if (or (is-owner tx-sender token-id)
              (is-operator-approved
               (unwrap! (get-owner token-id) nft-not-found-err)
               tx-sender))
          (begin
            (map-set tokens-spender
                        token-id
                        spender)
            (ok token-id))
          not-approved-spender-err)))

;; Sets or unsets the approval of a given operator
(define-public (set-operator-approval (operator principal) (is-approved bool))
  (if (is-eq operator tx-sender)
      sender-equals-recipient-err
      (begin
        (map-set accounts-operator
                    {operator: operator, account: tx-sender}
                    {is-approved: is-approved})
        (ok true))))

;; Transfers the ownership of a given token ID to another address
(define-public (transfer-from (token-id uint) (owner principal) (recipient principal))
  (begin
    (asserts! (can-transfer tx-sender token-id) not-approved-spender-err)
    (asserts! (is-owner owner token-id) nft-not-owned-err)
    (asserts! (not (is-eq recipient owner)) sender-equals-recipient-err)
    (transfer-token token-id owner recipient)))

;; Transfers tokens to a specified principal
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (transfer-from token-id tx-sender recipient))

;; Gets the owner of the specified token ID
(define-read-only (get-owner (token-id uint))
  (ok (unwrap! (nft-get-owner? my-nft token-id) (err u0))))

;; Gets the last token ID
(define-read-only (get-last-token-id)
  (ok u7))

;; Gets the URI associated with the specified token ID
(define-read-only (get-token-uri (token-id uint))
  (ok (some "Your_Metadata_URI")))

;; Error handling
(define-constant nft-not-owned-err (err u401))
(define-constant not-approved-spender-err (err u403))
(define-constant nft-not-found-err (err u404))
(define-constant sender-equals-recipient-err (err u405))
(define-constant nft-exists-err (err u409))

(define-map err-strings (response uint uint) (string-ascii 32))
(map-insert err-strings nft-not-owned-err "nft-not-owned")
(map-insert err-strings not-approved-spender-err "not-approaved-spender")
(map-insert err-strings nft-not-found-err "nft-not-found")
(map-insert err-strings nft-exists-err "nft-exists")

(define-private (nft-transfer-err (code uint))
  (if (is-eq u1 code)
    nft-not-owned-err
    (if (is-eq u2 code)
      sender-equals-recipient-err
      (if (is-eq u3 code)
        nft-not-found-err
        (err code)))))

(define-private (nft-mint-err (code uint))
  (if (is-eq u1 code)
    nft-exists-err
    (err code)))

(define-read-only (get-errstr (code uint))
  (unwrap! (map-get? err-strings (err code)) "unknown-error"))

;; Initialize

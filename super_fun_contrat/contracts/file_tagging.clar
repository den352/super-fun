;; Enhanced FileTagging Smart Contract
;; Purpose: Comprehensive file tagging system with advanced features

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-input (err u104))
(define-constant err-insufficient-payment (err u105))
(define-constant err-contract-disabled (err u106))
(define-constant err-quota-exceeded (err u107))
(define-constant err-invalid-permission (err u108))
(define-constant err-file-locked (err u109))
(define-constant err-subscription-expired (err u110))

;; Maximum limits
(define-constant max-tags-per-file u50)
(define-constant max-files-per-user u500)
(define-constant max-collaborators u10)
(define-constant max-file-size u1000000) ;; 1MB in bytes
(define-constant max-tag-length u32)
(define-constant max-filename-length u256)
(define-constant max-description-length u500)

;; Fee structure (in microSTX)
(define-constant base-fee u1000)
(define-constant premium-tag-fee u5000)
(define-constant bulk-operation-fee u10000)

;; File type constants
(define-constant file-type-document u1)
(define-constant file-type-image u2)
(define-constant file-type-video u3)
(define-constant file-type-audio u4)
(define-constant file-type-archive u5)
(define-constant file-type-other u6)

;; Permission constants
(define-constant permission-read u1)
(define-constant permission-write u2)
(define-constant permission-admin u3)

;; Data Variables
(define-data-var contract-enabled bool true)
(define-data-var total-files uint u0)
(define-data-var total-tags uint u0)
(define-data-var premium-features-enabled bool true)
(define-data-var contract-fees-enabled bool true)

;; Data Maps

;; Enhanced file information
(define-map files
  { file-id: (string-ascii 64) }
  {
    owner: principal,
    name: (string-utf8 256),
    description: (string-utf8 500),
    file-type: uint,
    file-size: uint,
    mime-type: (string-ascii 64),
    hash: (string-ascii 64),
    is-public: bool,
    is-locked: bool,
    is-archived: bool,
    created-at: uint,
    updated-at: uint,
    last-accessed: uint,
    access-count: uint,
    version: uint
  }
)

;; File tags with enhanced metadata
(define-map file-tags
  { file-id: (string-ascii 64), tag: (string-ascii 32) }
  {
    added-by: principal,
    added-at: uint,
    is-verified: bool,
    confidence-score: uint,
    source: (string-ascii 32) ;; manual, auto, ai, imported
  }
)

;; File tag lists for efficient retrieval
(define-map file-tag-list
  { file-id: (string-ascii 64) }
  { tags: (list 50 (string-ascii 32)) }
)

;; Tag file lists for tag-based search
(define-map tag-files
  { tag: (string-ascii 32) }
  { files: (list 100 (string-ascii 64)) }
)

;; User file lists
(define-map user-files
  { user: principal }
  { files: (list 500 (string-ascii 64)) }
)

;; Tag categories and hierarchies
(define-map tag-categories
  { tag: (string-ascii 32) }
  {
    category: (string-ascii 32),
    parent-tag: (optional (string-ascii 32)),
    color: (string-ascii 7), ;; hex color
    icon: (string-ascii 32),
    is-system-tag: bool,
    is-premium: bool
  }
)

;; File sharing and collaboration
(define-map file-permissions
  { file-id: (string-ascii 64), user: principal }
  {
    permission-level: uint,
    granted-by: principal,
    granted-at: uint,
    expires-at: (optional uint)
  }
)

;; User profiles and preferences
(define-map user-profiles
  { user: principal }
  {
    display-name: (string-utf8 64),
    avatar-url: (string-utf8 256),
    bio: (string-utf8 256),
    is-verified: bool,
    subscription-tier: uint, ;; 0=free, 1=premium, 2=enterprise
    subscription-expires: (optional uint),
    storage-used: uint,
    storage-limit: uint,
    created-at: uint,
    last-active: uint
  }
)

;; Enhanced tag statistics
(define-map tag-stats
  { tag: (string-ascii 32) }
  {
    usage-count: uint,
    unique-users: uint,
    first-used: uint,
    last-used: uint,
    trending-score: uint,
    weekly-usage: uint,
    monthly-usage: uint
  }
)

;; Collections/folders
(define-map collections
  { collection-id: (string-ascii 64) }
  {
    owner: principal,
    name: (string-utf8 128),
    description: (string-utf8 256),
    is-public: bool,
    created-at: uint,
    file-count: uint
  }
)

(define-map collection-files
  { collection-id: (string-ascii 64) }
  { files: (list 100 (string-ascii 64)) }
)

;; File ratings and reviews
(define-map file-ratings
  { file-id: (string-ascii 64), user: principal }
  {
    rating: uint, ;; 1-5 stars
    review: (string-utf8 256),
    created-at: uint,
    updated-at: uint
  }
)

;; Private helper functions

;; Check if a file has a specific tag
(define-private (has-tag (file-id (string-ascii 64)) (tag (string-ascii 32)))
  (is-some (map-get? file-tags { file-id: file-id, tag: tag }))
)

;; Add file to user's file list
(define-private (add-file-to-user (user principal) (file-id (string-ascii 64)))
  (let ((current-files-data (default-to { files: (list) } (map-get? user-files { user: user })))
        (current-files (get files current-files-data)))
    (if (is-none (index-of current-files file-id))
      (begin
        (map-set user-files
          { user: user }
          { files: (unwrap! (as-max-len? (append current-files file-id) u500) (err u999)) }
        )
        (ok true)
      )
      (ok true)
    )
  )
)

;; Add file to tag's file list
(define-private (add-file-to-tag (tag (string-ascii 32)) (file-id (string-ascii 64)))
  (let ((current-files-data (default-to { files: (list) } (map-get? tag-files { tag: tag })))
        (current-files (get files current-files-data)))
    (if (is-none (index-of current-files file-id))
      (begin
        (map-set tag-files
          { tag: tag }
          { files: (unwrap! (as-max-len? (append current-files file-id) u100) (err u999)) }
        )
        (ok true)
      )
      (ok true)
    )
  )
)

;; Add tag to file's tag list
(define-private (add-tag-to-file (file-id (string-ascii 64)) (tag (string-ascii 32)))
  (let ((current-tags-data (default-to { tags: (list) } (map-get? file-tag-list { file-id: file-id })))
        (current-tags (get tags current-tags-data)))
    (if (is-none (index-of current-tags tag))
      (begin
        (map-set file-tag-list
          { file-id: file-id }
          { tags: (unwrap! (as-max-len? (append current-tags tag) u50) (err u999)) }
        )
        (ok true)
      )
      (ok true)
    )
  )
)

;; Update tag statistics
(define-private (update-tag-stats (tag (string-ascii 32)) (increment bool))
  (let ((current-stats (map-get? tag-stats { tag: tag }))
        (current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (match current-stats
      stats (begin
        (map-set tag-stats
          { tag: tag }
          {
            usage-count: (if increment (+ (get usage-count stats) u1) (- (get usage-count stats) u1)),
            unique-users: (get unique-users stats),
            first-used: (get first-used stats),
            last-used: (if increment current-time (get last-used stats)),
            trending-score: (get trending-score stats),
            weekly-usage: (get weekly-usage stats),
            monthly-usage: (get monthly-usage stats)
          }
        )
        (ok true)
      )
      (if increment
        (begin
          (map-set tag-stats
            { tag: tag }
            {
              usage-count: u1,
              unique-users: u1,
              first-used: current-time,
              last-used: current-time,
              trending-score: u1,
              weekly-usage: u1,
              monthly-usage: u1
            }
          )
          (ok true)
        )
        (ok false)
      )
    )
  )
)

;; Update file access statistics
(define-private (update-file-access (file-id (string-ascii 64)))
  (match (get-file file-id)
    file-info (begin
      (map-set files
        { file-id: file-id }
        (merge file-info {
          last-accessed: (unwrap-panic (get-block-info? time (- block-height u1))),
          access-count: (+ (get access-count file-info) u1)
        })
      )
      (ok true)
    )
    (ok false)
  )
)

;; Log file activity
(define-private (log-file-activity (file-id (string-ascii 64)) (action (string-ascii 32)) (details (string-utf8 256)))
  ;; Log activity for audit trail - simplified implementation
  (ok true)
)

;; Check if user is premium
(define-private (is-premium-user (user principal))
  (match (get-user-profile user)
    profile (> (get subscription-tier profile) u0)
    false
  )
)

;; Charge fee for operations
(define-private (charge-fee (amount uint))
  (if (var-get contract-fees-enabled)
    (stx-transfer? amount tx-sender contract-owner)
    (ok true)
  )
)

;; Check if user can create file
(define-private (can-create-file (user principal) (file-size uint))
  (match (get-user-profile user)
    profile (< (+ (get storage-used profile) file-size) (get storage-limit profile))
    true ;; Allow if no profile exists
  )
)

;; Check if user can modify file
(define-private (can-modify-file (file-id (string-ascii 64)) (user principal))
  (match (get-file file-id)
    file-info (or
      (is-eq user (get owner file-info))
      (has-file-permission file-id user permission-write)
    )
    false
  )
)

;; Read-only functions

;; Basic getters
(define-read-only (get-file (file-id (string-ascii 64)))
  (map-get? files { file-id: file-id })
)

(define-read-only (get-user-profile (user principal))
  (map-get? user-profiles { user: user })
)

(define-read-only (get-file-tags (file-id (string-ascii 64)))
  (default-to 
    { tags: (list) }
    (map-get? file-tag-list { file-id: file-id })
  )
)

(define-read-only (get-files-by-tag (tag (string-ascii 32)))
  (default-to
    { files: (list) }
    (map-get? tag-files { tag: tag })
  )
)

(define-read-only (get-user-files (user principal))
  (default-to
    { files: (list) }
    (map-get? user-files { user: user })
  )
)

(define-read-only (get-tag-stats (tag (string-ascii 32)))
  (map-get? tag-stats { tag: tag })
)

;; Check if a file has a specific tag (public version)
(define-read-only (file-has-tag (file-id (string-ascii 64)) (tag (string-ascii 32)))
  (has-tag file-id tag)
)

;; Permission checking
(define-read-only (can-access-file (file-id (string-ascii 64)) (user principal))
  (match (get-file file-id)
    file-info (or
      (is-eq user (get owner file-info))
      (get is-public file-info)
      (has-file-permission file-id user permission-read)
    )
    false
  )
)

(define-read-only (has-file-permission (file-id (string-ascii 64)) (user principal) (required-level uint))
  (match (map-get? file-permissions { file-id: file-id, user: user })
    permission (and
      (>= (get permission-level permission) required-level)
      (match (get expires-at permission)
        expiry (< (unwrap-panic (get-block-info? time (- block-height u1))) expiry)
        true
      )
    )
    false
  )
)

;; Analytics
(define-read-only (get-user-statistics (user principal))
  (let ((user-files-data (default-to { files: (list) } (map-get? user-files { user: user })))
        (user-file-list (get files user-files-data)))
    {
      total-files: (len user-file-list),
      total-tags-used: u0, ;; Simplified
      storage-used: u0, ;; Simplified
      files-shared: u0, ;; Simplified
      collections-created: u0 ;; Simplified
    }
  )
)

;; Public functions

;; Create file with advanced options
(define-public (create-file-advanced 
  (file-id (string-ascii 64))
  (name (string-utf8 256))
  (description (string-utf8 500))
  (file-type uint)
  (file-size uint)
  (mime-type (string-ascii 64))
  (hash (string-ascii 64))
  (is-public bool))
  
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (asserts! (var-get contract-enabled) err-contract-disabled)
    (asserts! (> (len file-id) u0) err-invalid-input)
    (asserts! (> (len name) u0) err-invalid-input)
    (asserts! (<= file-size max-file-size) err-invalid-input)
    (asserts! (is-none (get-file file-id)) err-already-exists)
    (asserts! (can-create-file tx-sender file-size) err-quota-exceeded)
    
    ;; Charge fee for file creation
    (try! (charge-fee base-fee))
    
    (map-set files
      { file-id: file-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        file-type: file-type,
        file-size: file-size,
        mime-type: mime-type,
        hash: hash,
        is-public: is-public,
        is-locked: false,
        is-archived: false,
        created-at: current-time,
        updated-at: current-time,
        last-accessed: current-time,
        access-count: u0,
        version: u1
      }
    )
    
    ;; Update counters
    (var-set total-files (+ (var-get total-files) u1))
    
    ;; Add to user files
    (unwrap! (add-file-to-user tx-sender file-id) err-quota-exceeded)
    
    ;; Log activity
    (unwrap! (log-file-activity file-id "created" u"File created") (err u999))
    
    (ok file-id)
  )
)

;; Simple file creation (backward compatibility)
(define-public (create-file (file-id (string-ascii 64)) (name (string-utf8 256)))
  (create-file-advanced file-id name u"" file-type-other u0 "application/octet-stream" "" false)
)

;; Add a tag to a file
(define-public (add-tag (file-id (string-ascii 64)) (tag (string-ascii 32)))
  (let ((file-info (unwrap! (get-file file-id) err-not-found)))
    (asserts! (var-get contract-enabled) err-contract-disabled)
    (asserts! (> (len tag) u0) err-invalid-input)
    (asserts! (can-modify-file file-id tx-sender) err-unauthorized)
    (asserts! (not (has-tag file-id tag)) err-already-exists)
    
    ;; Add tag association
    (map-set file-tags
      { file-id: file-id, tag: tag }
      {
        added-by: tx-sender,
        added-at: (unwrap-panic (get-block-info? time (- block-height u1))),
        is-verified: false,
        confidence-score: u100,
        source: "manual"
      }
    )
    
    ;; Update data structures
    (unwrap! (add-tag-to-file file-id tag) err-quota-exceeded)
    (unwrap! (add-file-to-tag tag file-id) err-quota-exceeded)
    (unwrap! (update-tag-stats tag true) (err u999))
    
    ;; Log activity
    (unwrap! (log-file-activity file-id "tagged" u"Tag added") (err u999))
    
    (ok true)
  )
)

;; Remove a tag from a file
(define-public (remove-tag (file-id (string-ascii 64)) (tag (string-ascii 32)))
  (let ((file-info (unwrap! (get-file file-id) err-not-found)))
    (asserts! (var-get contract-enabled) err-contract-disabled)
    (asserts! (can-modify-file file-id tx-sender) err-unauthorized)
    (asserts! (has-tag file-id tag) err-not-found)
    
    ;; Remove tag association
    (map-delete file-tags { file-id: file-id, tag: tag })
    
    ;; Update tag statistics
    (unwrap! (update-tag-stats tag false) (err u999))
    
    ;; Log activity
    (unwrap! (log-file-activity file-id "untagged" u"Tag removed") (err u999))
    
    (ok true)
  )
)

;; Share file with specific permissions
(define-public (share-file 
  (file-id (string-ascii 64))
  (recipient principal)
  (permission-level uint)
  (expires-in-days (optional uint)))
  
  (let ((file-info (unwrap! (get-file file-id) err-not-found))
        (current-time (unwrap-panic (get-block-info? time (- block-height u1))))
        (expiry-time (match expires-in-days
          days (some (+ current-time (* days u86400))) ;; days to seconds
          none)))
    
    (asserts! (var-get contract-enabled) err-contract-disabled)
    (asserts! (is-eq tx-sender (get owner file-info)) err-unauthorized)
    (asserts! (<= permission-level permission-admin) err-invalid-permission)
    
    (map-set file-permissions
      { file-id: file-id, user: recipient }
      {
        permission-level: permission-level,
        granted-by: tx-sender,
        granted-at: current-time,
        expires-at: expiry-time
      }
    )
    
    ;; Log activity
    (unwrap! (log-file-activity file-id "shared" u"File shared with user") (err u999))
    
    (ok true)
  )
)

;; Rate and review file
(define-public (rate-file (file-id (string-ascii 64)) (rating uint) (review (string-utf8 256)))
  (let ((current-time (unwrap-panic (get-block-info? time (- block-height u1)))))
    (asserts! (var-get contract-enabled) err-contract-disabled)
    (asserts! (can-access-file file-id tx-sender) err-unauthorized)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-input)
    
    (map-set file-ratings
      { file-id: file-id, user: tx-sender }
      {
        rating: rating,
        review: review,
        created-at: current-time,
        updated-at: current-time
      }
    )
    
    (ok true)
  )
)

;; Admin functions

;; Toggle contract enabled state
(define-public (toggle-contract)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-enabled (not (var-get contract-enabled)))
    (ok (var-get contract-enabled))
  )
)

;; Emergency function to remove inappropriate tags (admin only)
(define-public (admin-remove-tag (file-id (string-ascii 64)) (tag (string-ascii 32)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (has-tag file-id tag) err-not-found)
    
    ;; Remove tag association
    (map-delete file-tags { file-id: file-id, tag: tag })
    
    ;; Update tag statistics
    (unwrap! (update-tag-stats tag false) (err u999))
    
    ;; Log activity
    (unwrap! (log-file-activity file-id "admin-removed-tag" u"Tag removed by admin") (err u999))
    
    (ok true)
  )
)
;; Accommodation Certification Contract
;; Verifies that hotels and lodging meet environmental standards

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-INVALID-INPUT (err u401))
(define-constant ERR-ACCOMMODATION-NOT-FOUND (err u402))
(define-constant ERR-CERTIFICATION-EXPIRED (err u403))
(define-constant ERR-ALREADY-EXISTS (err u404))

;; Data Variables
(define-data-var next-accommodation-id uint u1)
(define-data-var next-audit-id uint u1)
(define-data-var certification-validity-period uint u52560) ;; ~1 year in blocks

;; Data Maps
(define-map accommodations
  { accommodation-id: uint }
  {
    name: (string-ascii 100),
    location: (string-ascii 100),
    accommodation-type: (string-ascii 50), ;; hotel, guesthouse, eco-lodge, etc.
    capacity: uint,
    owner: principal,
    is-active: bool,
    registered-at: uint
  }
)

(define-map environmental-standards
  { accommodation-id: uint }
  {
    energy-efficiency-score: uint, ;; 1-100
    water-conservation-score: uint, ;; 1-100
    waste-management-score: uint, ;; 1-100
    renewable-energy-usage: uint, ;; percentage
    carbon-footprint-rating: uint, ;; 1-10 (lower is better)
    local-sourcing-percentage: uint, ;; percentage of local products used
    overall-sustainability-score: uint, ;; calculated average
    last-updated: uint
  }
)

(define-map certifications
  { accommodation-id: uint }
  {
    certification-level: (string-ascii 50), ;; bronze, silver, gold, platinum
    certification-score: uint,
    issued-at: uint,
    expires-at: uint,
    is-valid: bool,
    certified-by: principal
  }
)

(define-map audit-records
  { audit-id: uint }
  {
    accommodation-id: uint,
    auditor: principal,
    audit-type: (string-ascii 50), ;; initial, renewal, complaint-based
    energy-score: uint,
    water-score: uint,
    waste-score: uint,
    compliance-issues: uint,
    recommendations: (string-ascii 500),
    audit-date: uint,
    is-passed: bool
  }
)

(define-map authorized-auditors
  { auditor: principal }
  {
    is-authorized: bool,
    specialization: (string-ascii 100),
    certification-count: uint,
    authorized-at: uint
  }
)

;; Authorization check
(define-private (is-authorized-auditor (caller principal))
  (match (map-get? authorized-auditors { auditor: caller })
    auditor (get is-authorized auditor)
    false
  )
)

(define-private (is-accommodation-owner (accommodation-id uint) (caller principal))
  (match (map-get? accommodations { accommodation-id: accommodation-id })
    accommodation (is-eq (get owner accommodation) caller)
    false
  )
)

;; Public Functions

;; Register accommodation
(define-public (register-accommodation
  (name (string-ascii 100))
  (location (string-ascii 100))
  (accommodation-type (string-ascii 50))
  (capacity uint))
  (let ((accommodation-id (var-get next-accommodation-id)))
    (asserts! (> (len name) u0) ERR-INVALID-INPUT)
    (asserts! (> capacity u0) ERR-INVALID-INPUT)

    (map-set accommodations
      { accommodation-id: accommodation-id }
      {
        name: name,
        location: location,
        accommodation-type: accommodation-type,
        capacity: capacity,
        owner: tx-sender,
        is-active: true,
        registered-at: block-height
      }
    )

    ;; Initialize environmental standards with default values
    (map-set environmental-standards
      { accommodation-id: accommodation-id }
      {
        energy-efficiency-score: u0,
        water-conservation-score: u0,
        waste-management-score: u0,
        renewable-energy-usage: u0,
        carbon-footprint-rating: u10,
        local-sourcing-percentage: u0,
        overall-sustainability-score: u0,
        last-updated: block-height
      }
    )

    (var-set next-accommodation-id (+ accommodation-id u1))
    (ok accommodation-id)
  )
)

;; Update environmental standards
(define-public (update-environmental-standards
  (accommodation-id uint)
  (energy-efficiency uint)
  (water-conservation uint)
  (waste-management uint)
  (renewable-energy uint)
  (carbon-footprint uint)
  (local-sourcing uint))
  (let ((accommodation (unwrap! (map-get? accommodations { accommodation-id: accommodation-id }) ERR-ACCOMMODATION-NOT-FOUND)))
    (asserts! (or (is-accommodation-owner accommodation-id tx-sender)
                  (is-authorized-auditor tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (<= energy-efficiency u100) ERR-INVALID-INPUT)
    (asserts! (<= water-conservation u100) ERR-INVALID-INPUT)
    (asserts! (<= waste-management u100) ERR-INVALID-INPUT)
    (asserts! (<= renewable-energy u100) ERR-INVALID-INPUT)
    (asserts! (<= carbon-footprint u10) ERR-INVALID-INPUT)
    (asserts! (<= local-sourcing u100) ERR-INVALID-INPUT)
    (asserts! (get is-active accommodation) ERR-INVALID-INPUT)

    (let (
      (overall-score (/ (+ energy-efficiency water-conservation waste-management
                          renewable-energy (- u100 (* carbon-footprint u10)) local-sourcing) u6))
    )
      (map-set environmental-standards
        { accommodation-id: accommodation-id }
        {
          energy-efficiency-score: energy-efficiency,
          water-conservation-score: water-conservation,
          waste-management-score: waste-management,
          renewable-energy-usage: renewable-energy,
          carbon-footprint-rating: carbon-footprint,
          local-sourcing-percentage: local-sourcing,
          overall-sustainability-score: overall-score,
          last-updated: block-height
        }
      )

      (ok overall-score)
    )
  )
)

;; Conduct audit
(define-public (conduct-audit
  (accommodation-id uint)
  (audit-type (string-ascii 50))
  (energy-score uint)
  (water-score uint)
  (waste-score uint)
  (compliance-issues uint)
  (recommendations (string-ascii 500)))
  (let (
    (audit-id (var-get next-audit-id))
    (accommodation (unwrap! (map-get? accommodations { accommodation-id: accommodation-id }) ERR-ACCOMMODATION-NOT-FOUND))
    (auditor-info (unwrap! (map-get? authorized-auditors { auditor: tx-sender }) ERR-NOT-AUTHORIZED))
  )
    (asserts! (get is-authorized auditor-info) ERR-NOT-AUTHORIZED)
    (asserts! (<= energy-score u100) ERR-INVALID-INPUT)
    (asserts! (<= water-score u100) ERR-INVALID-INPUT)
    (asserts! (<= waste-score u100) ERR-INVALID-INPUT)
    (asserts! (get is-active accommodation) ERR-INVALID-INPUT)

    (let (
      (average-score (/ (+ energy-score water-score waste-score) u3))
      (is-passed (and (>= average-score u60) (<= compliance-issues u3)))
    )
      (map-set audit-records
        { audit-id: audit-id }
        {
          accommodation-id: accommodation-id,
          auditor: tx-sender,
          audit-type: audit-type,
          energy-score: energy-score,
          water-score: water-score,
          waste-score: waste-score,
          compliance-issues: compliance-issues,
          recommendations: recommendations,
          audit-date: block-height,
          is-passed: is-passed
        }
      )

      ;; Update auditor certification count
      (map-set authorized-auditors
        { auditor: tx-sender }
        (merge auditor-info { certification-count: (+ (get certification-count auditor-info) u1) })
      )

      (var-set next-audit-id (+ audit-id u1))
      (ok { audit-id: audit-id, passed: is-passed, score: average-score })
    )
  )
)

;; Issue certification
(define-public (issue-certification (accommodation-id uint) (audit-id uint))
  (let (
    (accommodation (unwrap! (map-get? accommodations { accommodation-id: accommodation-id }) ERR-ACCOMMODATION-NOT-FOUND))
    (audit (unwrap! (map-get? audit-records { audit-id: audit-id }) ERR-INVALID-INPUT))
    (standards (unwrap! (map-get? environmental-standards { accommodation-id: accommodation-id }) ERR-ACCOMMODATION-NOT-FOUND))
  )
    (asserts! (is-authorized-auditor tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get accommodation-id audit) accommodation-id) ERR-INVALID-INPUT)
    (asserts! (get is-passed audit) ERR-INVALID-INPUT)
    (asserts! (get is-active accommodation) ERR-INVALID-INPUT)

    (let (
      (certification-score (get overall-sustainability-score standards))
      (certification-level (if (>= certification-score u90) "platinum"
                           (if (>= certification-score u80) "gold"
                           (if (>= certification-score u70) "silver"
                           "bronze"))))
      (expires-at (+ block-height (var-get certification-validity-period)))
    )
      (map-set certifications
        { accommodation-id: accommodation-id }
        {
          certification-level: certification-level,
          certification-score: certification-score,
          issued-at: block-height,
          expires-at: expires-at,
          is-valid: true,
          certified-by: tx-sender
        }
      )

      (ok { level: certification-level, score: certification-score, expires-at: expires-at })
    )
  )
)

;; Renew certification
(define-public (renew-certification (accommodation-id uint) (new-audit-id uint))
  (let (
    (current-cert (unwrap! (map-get? certifications { accommodation-id: accommodation-id }) ERR-ACCOMMODATION-NOT-FOUND))
    (audit (unwrap! (map-get? audit-records { audit-id: new-audit-id }) ERR-INVALID-INPUT))
  )
    (asserts! (is-authorized-auditor tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get accommodation-id audit) accommodation-id) ERR-INVALID-INPUT)
    (asserts! (get is-passed audit) ERR-INVALID-INPUT)
    (asserts! (get is-valid current-cert) ERR-INVALID-INPUT)

    (let (
      (new-score (/ (+ (get energy-score audit) (get water-score audit) (get waste-score audit)) u3))
      (new-level (if (>= new-score u90) "platinum"
                 (if (>= new-score u80) "gold"
                 (if (>= new-score u70) "silver"
                 "bronze"))))
      (new-expires-at (+ block-height (var-get certification-validity-period)))
    )
      (map-set certifications
        { accommodation-id: accommodation-id }
        (merge current-cert {
          certification-level: new-level,
          certification-score: new-score,
          issued-at: block-height,
          expires-at: new-expires-at
        })
      )

      (ok { level: new-level, score: new-score, expires-at: new-expires-at })
    )
  )
)

;; Authorize auditor
(define-public (authorize-auditor (auditor principal) (specialization (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> (len specialization) u0) ERR-INVALID-INPUT)

    (map-set authorized-auditors
      { auditor: auditor }
      {
        is-authorized: true,
        specialization: specialization,
        certification-count: u0,
        authorized-at: block-height
      }
    )

    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-accommodation (accommodation-id uint))
  (map-get? accommodations { accommodation-id: accommodation-id })
)

(define-read-only (get-environmental-standards (accommodation-id uint))
  (map-get? environmental-standards { accommodation-id: accommodation-id })
)

(define-read-only (get-certification (accommodation-id uint))
  (map-get? certifications { accommodation-id: accommodation-id })
)

(define-read-only (get-audit-record (audit-id uint))
  (map-get? audit-records { audit-id: audit-id })
)

(define-read-only (get-auditor-info (auditor principal))
  (map-get? authorized-auditors { auditor: auditor })
)

(define-read-only (is-certification-valid (accommodation-id uint))
  (match (map-get? certifications { accommodation-id: accommodation-id })
    cert (and (get is-valid cert) (> (get expires-at cert) block-height))
    false
  )
)

(define-read-only (get-certification-level (accommodation-id uint))
  (match (map-get? certifications { accommodation-id: accommodation-id })
    cert (if (and (get is-valid cert) (> (get expires-at cert) block-height))
             (some (get certification-level cert))
             none)
    none
  )
)

(define-read-only (get-next-accommodation-id)
  (var-get next-accommodation-id)
)

(define-read-only (get-next-audit-id)
  (var-get next-audit-id)
)


;; genesis.layer.2.scm
;; Layer 2: structural addressing (IPv6-like 8×8bit)
;; No hashing. No content reads. Deterministic.

(define-module (tetragrammatron genesis layer2)
  #:use-module (ice-9 match)
  #:export (L2.assign
            L2.addr->hex
            L2.addr->vector))

;; --------------------------------------------------
;; helpers
;; --------------------------------------------------

(define (string-count s ch)
  (let loop ((i 0) (c 0))
    (if (= i (string-length s)) c
        (loop (+ i 1)
              (if (char=? (string-ref s i) ch) (+ c 1) c)))))

(define (ext-class path)
  (cond
    ((string-suffix? path ".scm") 1)
    ((string-suffix? path ".c")   2)
    ((string-suffix? path ".org") 3)
    ((string-suffix? path ".json") 4)
    ((string-suffix? path ".bin")  5)
    (else 0)))

(define (kind->byte kind)
  (case kind
    ((file) 1)
    ((dir)  2)
    ((symlink) 3)
    (else 0)))

;; --------------------------------------------------
;; core
;; --------------------------------------------------

;; l0 : alist from Layer 0
;; l1 : classification symbol
;; returns extended alist with 'addr
(define (L2.assign l0 l1)
  (let* ((path (cdr (assoc 'path l0)))
         (kind (cdr (assoc 'kind l0)))
         (depth (string-count path #\/))

         ;; fixed for now (can be schema-driven later)
         (realm   #x1A)
         (layer   2)

         (kind-b  (kind->byte kind))
         (ext-b   (ext-class path))

         ;; epoch is always monotone per run
         (epoch   0)

         ;; local index is derived from path ordering
         (index   (modulo (string-length path) 256)))

    `((addr .
        ,(vector
           realm         ; A0 realm
           layer         ; A1 genesis layer
           kind-b        ; A2 kind
           depth         ; A3 depth
           ext-b         ; A4 extension class
           epoch         ; A5 epoch
           index         ; A6 local index
           0))           ; A7 residue (filled later)
      (class . ,l1))))

;; --------------------------------------------------
;; formatting
;; --------------------------------------------------

(define (byte->hex b)
  (format #f "~2,'0X" b))

(define (L2.addr->hex addr)
  (string-join
    (map byte->hex (vector->list addr))
    ":"))

(define (L2.addr->vector addr)
  addr)
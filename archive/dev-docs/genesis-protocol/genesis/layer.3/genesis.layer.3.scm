
;; genesis.layer.3.scm
;; Layer 3: Org-mode provenance emitter
;; Append-only, deterministic, content-agnostic

(define-module (tetragrammatron genesis layer3)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (L3.emit-org
            L3.heading
            L3.properties))

;; --------------------------------------------------
;; helpers
;; --------------------------------------------------

(define (alist-ref k al)
  (let ((p (assoc k al)))
    (and p (cdr p))))

(define (prop-line k v)
  (format #f ":%s: %a
" k v))

;; --------------------------------------------------
;; property drawer
;; --------------------------------------------------

(define (L3.properties l0 l1 l2)
  (let* ((addr (alist-ref 'addr l2)))
    (string-append
     ":PROPERTIES:
"
     (prop-line "GENESIS_LAYER" 3)
     (prop-line "PATH"  (alist-ref 'path l0))
     (prop-line "KIND"  (alist-ref 'kind l0))
     (prop-line "CLASS" l1)
     (prop-line "ADDR"  (if addr
                             (string-join
                               (map (lambda (b) (format #f "~2,'0X" b))
                                    (vector->list addr))
                               ":")
                             "UNASSIGNED"))
     (prop-line "SIZE"        (alist-ref 'size l0))
     (prop-line "MTIME_SEC"   (alist-ref 'mtime_sec l0))
     (prop-line "MODE"        (alist-ref 'mode l0))
     (prop-line "UID"         (alist-ref 'uid l0))
     (prop-line "GID"         (alist-ref 'gid l0))
     ":END:
")))

;; --------------------------------------------------
;; heading
;; --------------------------------------------------

(define (L3.heading l0)
  (format #f "* %s
" (alist-ref 'path l0)))

;; --------------------------------------------------
;; emitter
;; --------------------------------------------------

(define (L3.emit-org l0 l1 l2)
  "Return a complete Org subtree as a string.
Caller decides where/how to append."
  (string-append
   (L3.heading l0)
   (L3.properties l0 l1 l2)
   "
<<PROVENANCE>>

"))
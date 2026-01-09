
;; genesis.layer.7.scm
;; Layer 7: semantic projection (Sphere boundary)

(define-module (tetragrammatron genesis layer7)
  #:use-module (ice-9 match)
  #:export (L7.residue
            L7.admissible?
            L7.project))

;; ---------------------------------------
;; residue computation
;; ---------------------------------------

(define (L7.residue addr)
  ;; addr is vector[8]
  ;; residue = A6 mod 8 (by design)
  (modulo (vector-ref addr 6) 8))

;; ---------------------------------------
;; admissibility policies
;; ---------------------------------------

(define (admissible-all-but-6 r)
  (not (= r 6)))

(define (admissible-odd-4 r)
  (member r '(1 3 5 7)))

(define (admissible-even-4 r)
  (member r '(0 2 4 6)))

(define (admissible-prime r)
  (member r '(2 3 5 7)))

(define (admissible-parity r)
  ;; always admissible, returns tag
  (if (even? r) 'even 'odd))

;; ---------------------------------------
;; policy dispatch
;; ---------------------------------------

(define (L7.admissible? r policy)
  (case policy
    ((all-but-6) (admissible-all-but-6 r))
    ((odd-4)     (admissible-odd-4 r))
    ((even-4)    (admissible-even-4 r))
    ((prime)     (admissible-prime r))
    ((parity)    #t)
    (else #f)))

;; ---------------------------------------
;; projection
;; ---------------------------------------

;; Input: l2 record with addr
;; Output: VM-visible state or #f
(define (L7.project l2 policy)
  (let* ((addr (cdr (assoc 'addr l2)))
         (r    (L7.residue addr)))
    (if (L7.admissible? r policy)
        `((vm.residue . ,r)
          (vm.policy  . ,policy))
        #f)))
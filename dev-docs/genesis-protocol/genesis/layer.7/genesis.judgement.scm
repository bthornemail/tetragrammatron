;; genesis.judgement.scm
;; Layer 7 judgement: verification without recomputation

(define-module (tetragrammatron genesis layer7 judgement)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:export (L7.judge))

;; --------------------------------------------------
;; helpers
;; --------------------------------------------------

(define (git-tracked? path)
  ;; returns #t if path is known to git (tracked or ignored explicitly)
  (let* ((p (open-pipe* OPEN_READ "git" "ls-files" "--error-unmatch" path))
         (out (read-line p 'concat)))
    (close-pipe p)
    (not (eof-object? out))))

(define (prop al k)
  (let ((p (assoc k al)))
    (and p (cdr p))))

;; --------------------------------------------------
;; judgement
;; --------------------------------------------------

;; Inputs:
;;  l0  : layer 0 stat alist
;;  l1  : classification symbol
;;  l2  : address alist
;;  org : parsed org property alist (from layer 3)
;;
;; Output: 'ok | 'warn | 'reject
;;
(define (L7.judge l0 l1 l2 org)
  (let* ((path (prop l0 'path))
         (addr (prop l2 'addr))
         (org-path (prop org 'PATH))
         (org-addr (prop org 'ADDR))
         (layer (prop org 'GENESIS_LAYER)))

    ;; --- Hard rejections ---
    (cond
      ;; missing address
      ((not addr) 'reject)

      ;; org path mismatch
      ((not (string=? path org-path)) 'reject)

      ;; layer regression
      ((not (<= 3 layer 7)) 'reject)

      ;; address collision / corruption
      ((not (string=? (L2.addr->hex addr) org-addr)) 'reject)

      ;; --- Soft warnings ---
      ((not (git-tracked? path))
       'warn)

      ;; --- Accept ---
      (else 'ok))))
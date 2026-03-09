
;; genesis.layer.0.scm  (Guile)
;; Layer 0: existence only (enumeration + stat)
;; No file content reads. No hashing. No parsing.

(define-module (tetragrammatron genesis layer0)
  #:use-module (ice-9 ftw)          ; file-system-walk
  #:use-module (ice-9 stat)         ; stat accessors
  #:use-module (ice-9 match)
  #:export (L0.enumerate L0.stat))

(define (->rel root path)
  (let* ((r (if (string-suffix? "/" root) root (string-append root "/"))))
    (if (string-prefix? r path)
        (substring path (string-length r) (string-length path))
        path)))

(define (kind-of st)
  (cond
    ((stat:directory? st) 'dir)
    ((stat:regular? st)   'file)
    ((stat:symlink? st)   'symlink)
    (else                 'other)))

(define (L0.stat root relpath)
  ;; MUST NOT read file bytes; only stat metadata.
  (let* ((abs (string-append (if (string-suffix? "/" root) root (string-append root "/")) relpath))
         (st  (stat abs)))
    `((path . ,relpath)
      (abs  . ,abs)
      (kind . ,(kind-of st))
      (size . ,(stat:size st))
      ;; keep mtime as integer seconds (stable, parseable)
      (mtime_sec . ,(stat:mtime st))
      (mode . ,(stat:perms st))
      (uid  . ,(stat:uid st))
      (gid  . ,(stat:gid st)))))

(define (L0.enumerate root)
  "Return a list of relative paths under root (files + dirs), excluding root itself."
  (let ((acc '()))
    (file-system-fold
      ;; enter?
      (lambda (path st result) #t)
      ;; leaf file
      (lambda (path st result)
        (set! acc (cons (->rel root path) acc))
        result)
      ;; down dir
      (lambda (path st result)
        (let ((rel (->rel root path)))
          (when (and (not (string=? rel "")) (not (string=? rel ".")))
            (set! acc (cons rel acc))))
        result)
      ;; up dir
      (lambda (path st result) result)
      ;; skip
      (lambda (path st result) result)
      ;; error
      (lambda (path err result) result)
      #f
      root)
    (reverse acc)))
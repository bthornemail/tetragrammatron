
;; genesis.layer.4.scm (Guile)
;; Layer 4: representation wrapper (bytes -> org/src blocks), no meaning.

(define-module (tetragrammatron genesis layer4)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:export (L4.ext->lang L4.represent L4.read-bytes-safe))

(define (string-suffix? s suf)
  (let ((ls (string-length s)) (lf (string-length suf)))
    (and (>= ls lf) (string=? (substring s (- ls lf) ls) suf))))

(define (L4.ext->lang path)
  (cond
    ((string-suffix? path ".scm") "scheme")
    ((string-suffix? path ".c") "c")
    ((string-suffix? path ".h") "c")
    ((string-suffix? path ".json") "json")
    ((string-suffix? path ".jsonl") "json")
    ((string-suffix? path ".yaml") "yaml")
    ((string-suffix? path ".yml") "yaml")
    ((string-suffix? path ".org") "org")
    ((string-suffix? path ".md") "markdown")
    ((string-suffix? path ".lean") "lean")
    (else "text")))

(define (L4.read-bytes-safe abs-path max-bytes)
  "Read up to max-bytes from abs-path. Returns (values kind payload)
kind ∈ 'utf8 'binary 'truncated. payload is a string for utf8, or bytevector for binary."
  (call-with-input-file abs-path
    (lambda (port)
      (set-port-encoding! port "UTF-8")
      (set-port-conversion-strategy! port 'error)
      (let* ((bv (get-bytevector-n port max-bytes))
             (more? (not (eof-object? (lookahead-u8 port)))))
        (if (or (eof-object? bv) (= (bytevector-length bv) 0))
            (values 'utf8 "")
            (let ((s (false-if-exception (utf8->string bv))))
              (cond
                (s (values (if more? 'truncated 'utf8) s))
                (else (values (if more? 'truncated 'binary) bv)))))))))

(define (L4.represent l0 l1 l2)
  ;; l0 contains 'abs and 'path
  (let* ((path (cdr (assoc 'path l0)))
         (abs  (cdr (assoc 'abs  l0)))
         (lang (L4.ext->lang path))
         (size (cdr (assoc 'size l0))))
    (cond
      ((eq? l1 'unknown-unknown)
       '((rep.kind . none)))

      ((eq? l1 'known-unknown)
       `((rep.kind . org-src)
         (rep.lang . ,lang)
         (rep.tangle . ,path)
         (rep.body . "<<CONTENT-DEFERRED>>")
         (rep.size . ,size)
         (rep.encoding . "unknown")))

      ((eq? l1 'known-known)
       (call-with-values
         (lambda () (L4.read-bytes-safe abs 1048576)) ;; 1 MiB cap
         (lambda (k payload)
           `((rep.kind . org-src)
             (rep.lang . ,lang)
             (rep.tangle . ,path)
             (rep.body . ,payload)
             (rep.size . ,size)
             (rep.encoding . ,(symbol->string k))))))

      (else '((rep.kind . none))))))
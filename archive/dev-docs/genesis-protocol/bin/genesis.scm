;; genesis.scm - minimal Guile adapter sketch (optional)
(define (usage) (display "usage: guile genesis.scm atom PATH\n") (exit 1))

(define (json-escape s)
  (let loop ((i 0) (out '()))
    (if (>= i (string-length s))
        (list->string (reverse out))
        (let ((c (string-ref s i)))
          (loop (+ i 1)
                (append
                 (cond
                  ((char=? c #\\) (string->list "\\\\"))
                  ((char=? c #\") (string->list "\\\""))
                  ((char=? c #\newline) (string->list "\\n"))
                  ((char=? c #\tab) (string->list "\\t"))
                  (else (list c)))
                 out))))))

(define (now-iso)
  (let* ((t (current-time)) (tm (gmtime t)))
    (strftime "%Y-%m-%dT%H:%M:%SZ" tm)))

(define (atom path)
  ;; Replace with (stat path) fields when you want the Guile-native version.
  (format #t "{\"t\":\"~a\",\"k\":\"genesis.atom\",\"v\":{\"path\":\"~a\",\"stat\":{}}}\n"
          (now-iso) (json-escape path)))

(let ((args (command-line)))
  (cond
   ((< (length args) 3) (usage))
   ((string=? (list-ref args 1) "atom") (atom (list-ref args 2)))
   (else (usage))))

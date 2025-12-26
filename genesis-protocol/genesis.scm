;; genesis.scm  (Guile)
;; Usage:
;;   guile -s genesis.scm -- GENESIS <name> <language> <switches> <header-args> <body>
;;   guile -s genesis.scm -- genesis <name> <language> <switches> <header-args> <body>
;;
;; Any missing argument defaults to "00".
;; Separators are views; we emit a canonical ':' view by default.

(use-modules (ice-9 match)
             (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-13))

(define ZERO "00")

(define (present? x)
  (and x (not (string=? x ZERO)) (not (string-null? x))))

(define (bit x) (if (present? x) 1 0))

(define (pad5 args)
  "Return a 5-tuple list, padding missing with ZERO."
  (let loop ((xs args) (acc '()))
    (cond
      ((= (length acc) 5) (reverse acc))
      ((null? xs) (loop xs (cons ZERO acc)))
      (else (loop (cdr xs) (cons (car xs) acc))))))

(define (polarity-of cmd)
  "Uppercase implies authority; lowercase implies mirror."
  (if (and cmd (string=? cmd (string-upcase cmd)))
      'authority
      'mirror))

(define (canonical-tuple xs)
  "Canonical view uses ':' as separator. Identity never depends on ':'.
  This is a view-only rendering."
  (string-join xs ":"))

(define (bits-of xs)
  "Return bits as string b0b1b2b3b4."
  (let ((b (map bit xs)))
    (string-concatenate (map (lambda (n) (number->string n)) b))))

(define (popcount bits)
  (fold + 0 (map (lambda (ch) (if (char=? ch #\1) 1 0))
                 (string->list bits))))

(define (class-of degree)
  (cond
    ((= degree 0) "zero")
    ((= degree 1) "monomial")
    ((= degree 2) "binomial")
    ((= degree 3) "trinomial")
    ((= degree 4) "4-term")
    ((= degree 5) "5-term")
    (else "unknown")))

(define (simple-atom-placeholder tuple bits)
  "Deterministic placeholder atom.
   Replace this later with normalize(stat ⊕ tuple ⊕ bits).
   Constraint: atom must be alphanumeric-only.
   Here we just strip non-alnum and take a stable prefix."
  (let* ((raw (string-append tuple "|" bits))
         (alnum (list->string
                 (filter (lambda (c)
                           (or (char-alphabetic? c) (char-numeric? c)))
                         (string->list raw)))))
    ;; keep it reasonably short for display; you can remove this.
    (if (> (string-length alnum) 24)
        (substring alnum 0 24)
        alnum)))

(define (emit-record cmd xs)
  (let* ((pol (polarity-of cmd))
         (tuple (canonical-tuple xs))
         (bits (bits-of xs))
         (deg (popcount bits))
         (klass (class-of deg))
         (atom (simple-atom-placeholder tuple bits)))
    (format #t "GENESIS_RECORD\n")
    (format #t "  polarity: ~a\n" pol)
    (format #t "  tuple:    ~a\n" tuple)
    (format #t "  bits:     ~a\n" bits)
    (format #t "  degree:   ~a\n" deg)
    (format #t "  class:    ~a\n" klass)
    (format #t "  atom:     ~a\n" atom)
    (format #t "\n")
    (format #t "VIEWS (equivalent):\n")
    (format #t "  .genesis/~a.genesis\n" atom)
    (format #t "  ~a.~a\n" (if (eq? pol 'authority) "GENESIS" "genesis") "org")
    (format #t "  ~a\n" (string-upcase atom))
    (format #t "  ~a\n" (string-downcase atom))
    (format #t "\n")))

(define (main argv)
  (match argv
    ((_ "--" cmd . rest)
     (let ((xs (pad5 rest)))
       (emit-record cmd xs)))
    ((_ cmd . rest)
     (let ((xs (pad5 rest)))
       (emit-record cmd xs)))
    (_
     (format #t "Usage:\n")
     (format #t "  guile -s genesis.scm -- GENESIS <name> <language> <switches> <header-args> <body>\n")
     (format #t "  guile -s genesis.scm -- genesis <name> <language> <switches> <header-args> <body>\n")
     (format #t "Missing args default to \"00\".\n"))))

(main (command-line))

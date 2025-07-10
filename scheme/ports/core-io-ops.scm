;;; Core I/O Operations -------------------------------------------------------
;; Part of scsh 1.0. See file COPYING for notices and license.
;; 
;; Redefines scheme48's `read-char`, `peek-char`, `write-char` and core i/o ops 
;; that rely on them. The reason they need to be redefined is becasuse s48
;; seeks to optimize these common operations by turning them into VM instructions, 
;; instead of consulting port-handler. We cannot mutate "stable" s48 structures
;; (i.e. any of built-in structures), so this seems to be the only way.
;; 
;; Additionally, we extend R5RS i/o operations to handle file descriptors. 

;;; Input Ops -----------------------------------------------------------------

(define-simple-syntax
  (define-r5rs-input (name arg ...) stream port-only? funname body ...)
  (define (name arg ... . maybe-i/o)
    (let ((stream (:optional maybe-i/o (current-input-port))))
      (cond ((open-input-port? stream) (funname arg ... stream))
            (port-only? (error "Requires fdport" stream))
            ((integer? stream) body ...)
            (else (error "Not a file descriptor or open input fdport" stream))))))

;; The MODE argument says whether we're doing a READ (#f) , a PEEK (#t),
;; or a CHAR-READY? ( '() ), as expected in the handler (see fdport-internal.scm)
(define (make-read-or-peek-char mode)
    (lambda (port)
        ((port-handler-char (port-handler port)) port mode)))
(define (real-read-char port)   (make-read-or-peek-char #t))
(define (real-char-ready? port) (make-read-or-peek-char '()))
(define (real-peek-char port)   (make-read-or-peek-char #f))

(define-r5rs-input (read-char) input #f real-read-char
  (let ((port (fdes->inport input)))
    (real-read-char port)))

(define-r5rs-input (char-ready?) input #f real-char-ready?
  (%char-ready-fdes? input))

(define-r5rs-input (peek-char) input #t real-peek-char)

; TODO (read) groan; only on ports because of peeking

;;; Output Ops ----------------------------------------------------------------

; TODO fix fdes in macro:
; (close-after (fdes->outport 3) (lambda (prt) (write-char #\~ prt)))
(define-simple-syntax
  (define-r5rs-output (name arg ...) stream body ...)
  (define (name arg ... . maybe-i/o)
    (if (integer? maybe-i/o)
        (close-after (fdes->outport maybe-i/o) 
                (lambda (prt) (let ((stream prt)) body ...)))
        (let ((stream (:optional maybe-i/o (current-output-port))))
          (cond ((open-output-port? stream) body ...)
                (else (error "Not a file descriptor or open output fdport" stream)))))))

(define-r5rs-output (write-char char) outport
  ((port-handler-char (port-handler outport)) outport char)
  (unspecific))
            
(define-r5rs-output (write-string s) outport
  (do ((i 0 (+ i 1)))
     ((= i (string-length s)))
   (write-char (string-ref s i) outport)))

(define-r5rs-output (newline) outport
  (write-char #\newline outport))

(define-r5rs-output (write obj) outport
  (if (and (output-port? outport)
	         (open-output-port? outport))
	    (let recur ((obj obj))
	      (recurring-write obj outport recur))
	    (assertion-violation 'write "invalid port argument" outport)))

(define-r5rs-output (display obj) outport
  (if (and (output-port? outport)
	         (open-output-port? outport))
      (let recur ((obj obj))
        (cond ((string? obj) (write-string obj outport))
              ((char? obj) (write-char obj outport))
              (else (recurring-write obj outport recur))))
	    (assertion-violation 'display "invalid port argument" outport)))

;; ----------------------------------------------------------------------------
;; Code below largely comes from scheme48's writing.scm  

(define (recurring-write obj port recur)
  (cond ((null? obj) (write-string "()" port))
        ((pair? obj) (write-list obj port recur))
        ((eq? obj #t) (write-boolean 't port))
        ((eq? obj #f) (write-boolean 'f port))
        ((symbol? obj) (write-string (symbol->string obj) port))
        ((number? obj) (write-number obj port))
        ((string? obj) (write-string-literal obj port))
        ((char? obj) (write-char-literal obj port))
	(else (write-other obj port recur))))

(define (write-boolean mumble port)
  (write-char #\# port)
  (write mumble port))

(define (write-number n port)
  (write-string (number->string n 10) port))

(define (write-char-literal obj port)
  (let ((probe (character-name obj)))
    (write-string "#\\" port)
    (if probe
	(write probe port)
	(write-char obj port))))

;; Don't use non-R5RS char literals to avoid bootstrap circularities
(define *nul* (scalar-value->char 0))
(define *alarm* (scalar-value->char 7))
(define *backspace* (scalar-value->char 8))
(define *tab* (scalar-value->char 9))
(define *linefeed* (scalar-value->char 10))
(define *vtab* (scalar-value->char 11))
(define *page* (scalar-value->char 12))
(define *return* (scalar-value->char 13))
(define *escape* (scalar-value->char 27))
(define *rubout* (scalar-value->char 127))

(define *char-name-table*
  (list
   (cons #\space 'space)
   (cons #\newline 'newline)
   (cons *nul* 'nul)
   (cons *alarm* 'alarm)
   (cons *backspace* 'backspace)
   (cons *tab* 'tab)
   (cons *linefeed* 'linefeed)
   (cons *vtab* 'vtab)
   (cons *page* 'page)
   (cons *return* 'return)
   (cons *escape* 'escape)
   (cons *rubout* 'rubout)))

(define (character-name char)
  (cond ((assq char *char-name-table*)
	 => cdr)
	(else #f)))

(define (write-string-literal obj port)
  (write-char #\" port)
  (let ((len (string-length obj)))
    (do ((i 0 (+ i 1)))
	((= i len) (write-char #\" port))
      (let ((c (string-ref obj i)))
	(cond
	 ((or (char=? c #\\) (char=? c #\"))
	  (write-char #\\ port)
	  (write-char c port))
	 ((char=? c *alarm*) (write-string "\\a" port))
	 ((char=? c *backspace*) (write-string "\\b" port))
	 ((char=? c *tab*) (write-string "\\t" port))
	 ((char=? c *linefeed*) (write-string "\\n" port))
	 ((char=? c *vtab*) (write-string "\\v" port))
	 ((char=? c *page*) (write-string "\\f" port))
	 ((char=? c *return*) (write-string "\\r" port))
	 ((char=? c *escape*)  (write-string "\\e" port))
	 (else
	  (write-char c port)))))))

(define (write-list obj port recur)
  (cond ((quotation? obj)
         (write-char #\' port)
         (recur (cadr obj)))
        (else
         (write-char #\( port)
         (recur (car obj))
         (let loop ((l (cdr obj))
                    (n 1))
              (cond ((not (pair? l))
                     (cond ((not (null? l))
                            (write-string " . " port)
                            (recur l))))
                    (else
                      (write-char #\space port)
                      (recur (car l))
                      (loop (cdr l) (+ n 1)))))
         (write-char #\) port))))

(define (quotation? obj)
  (and (pair? obj)
       (eq? (car obj) 'quote)
       (pair? (cdr obj))
       (null? (cddr obj))))

(define (write-vector obj port recur)
   (write-string "#(" port)
   (let ((z (vector-length obj)))
     (cond ((> z 0)
            (recur (vector-ref obj 0))
            (let loop ((i 1))
              (cond ((>= i z))
                    (else
                     (write-char #\space port)
                     (recur (vector-ref obj i))
                     (loop (+ i 1))))))))
   (write-char #\) port))

; The vector case goes last just so that this version of WRITE can be
; used in Scheme implementations in which records, ports, or
; procedures are represented as vectors.  (Scheme 48 doesn't have this
; property.)

(define (write-other obj port recur)
  (cond ((disclose obj)
	 => (lambda (l)
	      (write-string "#{" port)
	      (display (car l) port)
	      (for-each (lambda (x)
			  (write-char #\space port)
			  (recur x))
			(cdr l))
	      (write-string "}" port)))
	((eof-object? obj) (write-string "#{End-of-file}" port))
	((vector? obj) (write-vector obj port recur))
	((procedure? obj) (write-string "#{Procedure}" port))
	((code-vector? obj)
	 (write-string "#{Byte-vector}" port))
	((channel? obj)
	 (write-string "#{Channel " port)
	 (display (channel-id obj) port)
	 (write-string "}" port))
	((eq? obj (if #f #f)) (write-string "#{Unspecific}" port))
	(else
	 (write-string "#{Random object}" port))))




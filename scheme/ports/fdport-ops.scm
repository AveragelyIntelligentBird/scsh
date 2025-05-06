;;; Part of scsh 1.0. See file COPYING for notices and license.
;; user-facing fdport operations + syscalls


;;; Buffering policy setter
;; Works only on ports where i/o has not yet been started. Since we cannot set! a port's 
;; handler or buffer, we rely on auxilary data in channel-cell record to perform buffering control.
;; One unfortunate implication is that every port will have an internal buffer of size equal 
;; to max-soft-bufsize, regardless of the actual buffer size requested by user. 
(define (set-port-buffering port bufpol . maybe-buffer-size)
  (check-arg fdport? port set-port-buffering)
  (check-arg buf-policy? bufpol set-port-buffering)
  (let* ((input? (input-port? port))
         (buffer-size (if (null? maybe-buffer-size) 
                          max-soft-bufsize 
                          (car maybe-buffer-size)))
         (bufpol (cond ((and input? (= 1 buffer-size)) bufpol/none)
                       ((and (not input?) (= 0 buffer-size)) bufpol/none)
                       (else bufpol))))               
    (cond 
      ; Errors
      ((fdport-i/o-started? port)
          (error
            "Cannot set buffering policy on a port that already began i/o"
            port))
      ((> buffer-size max-soft-bufsize)
          (error
            "Given buffer size is bigger than the maximum buffer size"
            buffer-size '> max-soft-bufsize))
      ((or (not (integer? buffer-size)) 
           (and input? (>= 0 buffer-size))
           (and (not input?) (> 0 buffer-size)))
          (error 
            "Invalid buffer size for the given bufpol"
             bufpol buffer-size))
      ((and input? (buf-policy=? bufpol bufpol/line))
          (error 
            "Cannot set line buffering on input ports"
            port bufpol))
      (else 
        (reset-fdport-for-bufpol port bufpol buffer-size input?))))) 

;;; Open & Close
;;; ------------

; TODO implement our own open-file to avoid registering output port as "periodically flushed" at all smh
(define (open-file fname options . maybe-mode)
  (let* ((s48-port
          (with-resources-aligned
            (list cwd-resource umask-resource euid-resource egid-resource)
            (lambda ()
              (s48-open-file fname options (:optional maybe-mode (file-mode read write))))))
         (channel (s48-port->channel s48-port))
         (port (if (input-port? s48-port)
                    (make-input-fdport channel bufpol/block)
                    (make-output-fdport channel bufpol/block))))
    (set-fdport! (fdport->fd port) port 0)
    port))

(define (open-input-file fname . maybe-options)
  (let ((options (:optional maybe-options (file-options))))
    (open-file fname (file-options-union options (file-options read-only)))))

(define (open-output-file fname . rest)
  (let* ((options (if (pair? rest) (car rest)
                      (file-options create truncate))) ; default
         (maybe-mode (if (null? rest) '() (cdr rest)))
         (options (file-options-union options (file-options write-only))))
    (apply open-file fname options maybe-mode)))



(define (release-port-handle port)
  (check-arg fdport? port release-port-handle)
  (atomically!
   (let ((rev (fdport:revealed port)))
     (if (not (zero? rev))
         (let ((new-rev (- rev 1)))
           (set-fdport:revealed! port new-rev))))))

(define (port-revealed port)
  (let ((count (fdport:revealed (check-arg fdport? port port-revealed))))
    (and (not (zero? count)) count)))

(define (fdes->port fd port-maker) ; local proc.
  (cond  ((maybe-ref-fdport-port fd) =>
          (lambda (p)
            (increment-revealed-count p 1)
            p))
         (else (port-maker fd 1))))

(define (fdes->inport fd)
  (let ((port (fdes->port fd make-input-fdport/fd)))
    (if (not (input-port? port))
        (error "fdes was already assigned to an outport" fd)
        port)))

(define (fdes->outport fd)
  (let ((port (fdes->port fd make-output-fdport/fd)))
    (if (not (output-port? port))
        (error "fdes was already assigned to an inport" fd)
        port)))

; TODO move to separate file to disambiguate with fdport->fd
(define (port->fdes port)
  (check-arg open-fdport? port port->fdes)
  (increment-revealed-count port 1)
  (fdport->fd port))

(define (call/fdes fd/port proc)
  (cond ((integer? fd/port)
         (proc fd/port))
        ((fdport? fd/port)
         (let ((port fd/port))
           (dynamic-wind
            (lambda ()
              (if (not port) (error "Can't throw back into call/fdes.")))
            (lambda () (proc (port->fdes port)))
            (lambda ()
              (release-port-handle port)
              (set! port #f)))))
        (else (error "Not a file descriptor or fdport." fd/port))))

;;; Don't mess with the revealed count in the port case
;;; -- just sneakily grab the fdes and run.

(define (sleazy-call/fdes fd/port proc)
  (proc (cond ((integer? fd/port) fd/port)
              ((fdport->channel fd/port) (fdport->fd fd/port)) ;notice fd-port? instead of fdport?
              (else (error "Not a file descriptor or fdport." fd/port)))))


;;; Moves an i/o handle FD/PORT to fd TARGET.
;;; - If FD/PORT is a file descriptor, this is dup2(); close().
;;; - If FD/PORT is a port, this shifts the port's underlying file descriptor
;;;   to TARGET, as above, closing the old one. Port's revealed count is
;;;   set to 1.
;;; TARGET is evicted before the shift -- if there is a port allocated to
;;; file descriptor TARGET, it will be shifted to another file descriptor.

;; (define (move->fdes fd/port target)
;;   (let ((doit (lambda (fd)
;; 		(if (not (= fd target))
;; 		    (begin (evict-ports target) ; Evicts any ports at TARGET.
;; 			   (%dup2 fd target))))))

;;     (cond ((integer? fd/port)
;; 	   (doit fd/port)
;; 	   target)

;; 	  ((fdport? fd/port)
;; 	   (sleazy-call/fdes fd/port doit)
;; 	   (if (%move-fdport target fd/port 1)
;; 	       (error "fdport shift failed."))
;; 	   fd/port)

;; 	  (else (error "Argument not fdport or file descriptor" fd/port)))))

(define (move->fdes port target) ; TODO - revise, increments revelated count??
  (%dup2 (port->fdes port) target))

(define (input-source? fd/port)
  (check-arg fd/port? fd/port input-source?)
  (input-port? fd/port))

(define (output-source? fd/port)
  (check-arg fd/port? fd/port output-source?)
  (output-port? fd/port))


;;; If FD/PORT is a file descriptor, returns a file descriptor.
;;; If FD/PORT is a port, returns a port.

(define (dup fd/port . maybe-target)
  (check-arg fd/port? fd/port dup)
  (apply (cond ((integer? fd/port) dup->fdes)
	       ((input-port?  fd/port) dup->inport)
	       ((output-port? fd/port) dup->outport))
	 fd/port maybe-target))

(define (dup->fdes fd/port . maybe-target)
  (check-arg fd/port? fd/port dup->fdes)
  (if (pair? maybe-target)
      (let ((target (car maybe-target)))
	(close-fdes target)	; Thus evicting any port there.
	(sleazy-call/fdes fd/port (lambda (fd) (%dup2 fd target))))
      (sleazy-call/fdes fd/port %dup)))

(define (dup->inport fd/port . maybe-target)
  (apply really-dup->port make-input-fdport/fd fd/port maybe-target))

(define (dup->outport fd/port . maybe-target)
  (apply really-dup->port make-output-fdport/fd fd/port maybe-target))

(define (really-dup->port port-maker fd/port . maybe-target)
  (let ((fd (apply dup->fdes fd/port maybe-target)))
    (port-maker fd (if (null? maybe-target) 0 1))))

;;; Not exported.
(define (shell-open path flags fdes)
  (%dup2 (port->fdes (open-file (stringify path) flags (file-mode read write))) fdes))

(define create+trunc
  (file-options write-only create truncate))

(define write+append+create
  (file-options write-only append create))

(define read-only
  (file-options read-only))

;;; Generic port operations
;;; -----------------------

;;; (close-after port f)
;;;     Apply F to PORT. When F returns, close PORT, then return F's result.
;;;     Does nothing special if you throw out or throw in.

(define (close-after port f)
  (receive vals (f port)
    (close port)
    (apply values vals)))

(define (close port/fd)
  ((cond ((integer? port/fd)     close-fdes)
         ((output-port? port/fd) close-output-port)
         ((input-port?  port/fd) close-input-port)
         (else (error "Not file-descriptor or port" port/fd)))  port/fd))

;;; If this fd has an associated input or output port,
;;; move it to a new fd, freeing this one up.

(define (evict-ports fd)
  (cond ((maybe-ref-fdport-port fd) =>       ; Shouldn't bump the revealed count.
         (lambda (port)
             (%move-fdport (fdport->fd (s48-dup port)) port 0)  ;s48's dup modifies port's channel for us
             #t))
        (else #f)))

(define (%move-fdport old-fd port new-revealed)
  (delete-fdport! old-fd)
  (set-fdport! (fdport->fd port) port new-revealed)
  #f)  ; JMG: It used to return #f on succes in 0.5.1, so we do the same

(define (close-fdes fd)
  (if (evict-ports fd)
      #t ; EBADF should not occur if there is a port
      (%close-fdes fd)))

(define (flush-fdport fdport)
  (force-output (check-arg fdport? fdport flush-fdport)))

;;; Extend R4RS i/o ops to handle file descriptors.
;;; -----------------------------------------------
;;; Will probably need to limit buffers somehow..

(define-simple-syntax
  (define-r4rs-input (name arg ...) stream s48name body ...)
  (define (name arg ... . maybe-i/o)
    (let ((stream (:optional maybe-i/o (current-input-port))))
      (cond ((input-port? stream) (s48name arg ... stream))
            ((integer? stream) body ...)
            (else (error "Not a port or file descriptor" stream))))))

(define-r4rs-input (char-ready?) input s48-char-ready?
  (%char-ready-fdes? input))

(define-r4rs-input (read-char) input s48-read-char
  (let ((port (fdes->inport input)))
    (s48-read-char port)))

(define-simple-syntax
  (define-r4rs-output (name arg ...) stream s48name body ...)
  (define (name arg ... . maybe-i/o)
    (let ((stream (:optional maybe-i/o (current-output-port))))
      (cond ((output-port? stream) (s48name arg ... stream))
            ((integer? stream) body ...)
            (else (error "Not an outport or file descriptor" stream))))))

;; s48 has a bug: the default text codec for new ports is latin-1,
;; while the reference suggests it should be utf-8; we definitely prefer utf-8
;; We will manually reset the codec for port-creating forms exposed to user
(define (make-string-output-port) 
  (let ((sp (s48-make-string-output-port)))
    (set-port-text-codec! sp utf-8-codec) 
    sp))
;; For (make-string-input-port string), s48 *does* set encoding to utf-8 ¯\_(ツ)_/¯
;; (call-with-string-output-port proc) and (string-output-port-output port)
;;   implicitly read data as utf-8, so we fine 

;;; This one depends upon S48's string ports.
(define-r4rs-output (display object) output s48-display
  (let ((sp (make-string-output-port)))
    (display object sp)
    (write-string (string-output-port-output sp) output)))

(define-r4rs-output (newline) output s48-newline
  (let ((port (fdes->outport output)))
    (s48-newline port)))

(define-r4rs-output (write object) output s48-write
  (let ((sp (make-string-output-port)))
    (write object sp)
    (write-string (string-output-port-output sp) output)))

(define-r4rs-output (write-char char) output s48-write-char
  (let ((port (fdes->outport output)))
    (s48-write-char char port)))

; TODO replace the faulty write chars with an actually correct thing wat
(define (sanity-write-char char port) 
  ((port-handler-char (port-handler port)) port char)
  (unspecific))

(define (sanity-write-string string port) 
  (let ((write-ch (port-handler-char (port-handler port)))
        (len (string-length string)))
  (do ((i 0 (+ i 1)))
      ((>= i len))
        (write-ch port (string-ref string i)))
  (newline port)))

;;; S48's force-output doesn't default to forcing (current-output-port).
(define-r4rs-output (force-output) output s48-force-output
  (values)) ; Do nothing if applied to a file descriptor.

(define (format dest cstring . args)
  (if (integer? dest)
      (write-string (apply s48-format #f cstring args) dest)
      (apply s48-format dest cstring args)))

;;; with-current-foo-port procs
;;; ---------------------------

(define (with-current-input-port* port thunk)
  (call-with-current-input-port port thunk))

(define (with-current-output-port* port thunk)
  (call-with-current-output-port port thunk))

(define (with-current-error-port* port thunk)
  (call-with-current-noise-port port thunk))

(define (with-error-output-port* port thunk)
  (call-with-current-noise-port port thunk))

(define-simple-syntax (with-current-input-port port body ...)
  (with-current-input-port* port (lambda () body ...)))

(define-simple-syntax (with-current-output-port port body ...)
  (with-current-output-port* port (lambda () body ...)))

(define-simple-syntax (with-current-error-port port body ...)
  (with-current-error-port* port (lambda () body ...)))

(define-simple-syntax (with-error-output-port port body ...)
  (with-error-output-port* port (lambda () body ...)))

;;; I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define seek/set   0)        ; Unix codes for "whence"
(define seek/delta 1)
(define seek/end   2)

(define (seek fd offset . maybe-whence)
  (let ((whence (:optional maybe-whence seek/set))
        (fd (check-arg integer? fd seek)))
    (%fd-seek fd offset whence)))

(define (tell fd)
  (let ((fd (check-arg integer? fd tell)))
    (%fd-seek fd 0 seek/delta)))

(define (mumble-with-mumble-file open call)
  (lambda (string proc)
    (let ((port #f))
      (dynamic-wind
          (lambda () (set! port (open string)))
          (lambda () (call proc port))
          (lambda () (if port (close port)))))))

(define call-with-input-file
  (mumble-with-mumble-file open-input-file (lambda (proc port) (proc port))))

(define call-with-output-file
  (mumble-with-mumble-file open-output-file (lambda (proc port) (proc port))))

(define with-input-from-file
  (mumble-with-mumble-file open-input-file (lambda (thunk port) (call-with-current-input-port port thunk))))

(define with-output-to-file
  (mumble-with-mumble-file open-output-file (lambda (thunk port) (call-with-current-output-port port thunk))))

(import-lambda-definition-2 pipe-fdes () "scheme_pipe")

(define (pipe)
  (apply (lambda (r-fd w-fd)
           (let ((r (fdes->inport  r-fd))
                 (w (fdes->outport w-fd)))
             (release-port-handle r)
             (release-port-handle w)
             (values r w)))
         (pipe-fdes)))

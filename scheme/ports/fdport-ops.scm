;;; fdports operations --------------------------------------------------------
;; Part of scsh 0.7. See file COPYING for notices and license.
;; Contains high-level, user-facing fdport operations + syscalls

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
         (bufpol (cond ((and input? (<= 1 buffer-size)) bufpol/none)
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

;;; File open and close -> fdport
;;; ----------------------------------

(define (open-fdes fname flags . maybe-mode)
  (let* ((mode     (:optional maybe-mode (file-mode read write))) ; #o666 
         (mode-arg (cond ((file-mode? mode) mode)
                         ((integer? mode) (integer->file-mode mode))
                         (else (error "Invalid file mode (not number or file-mode)" mode)))))
    (with-resources-aligned
      (list cwd-resource umask-resource euid-resource egid-resource)
      (lambda () (%open fname flags mode-arg)))))

(define (open-file fname flags . maybe-mode)
  (let ((fd (apply open-fdes fname flags maybe-mode))
        (input? (file-flags-on? flags (file-flags read-only))))
    (if input?
      (make-input-fdport/fd  fd 0 fname)
      (make-output-fdport/fd fd 0 fname))))

(define (open-input-file fname . maybe-flags)
  (let* ((flags (:optional maybe-flags (file-flags)))
         (flags (file-flags+ flags (file-flags read-only))))
    (open-file fname flags)))

(define (open-output-file fname . rest)
  (let* ((flags (if (pair? rest) (car rest)
                      (file-flags create truncate))) ; default
         (flags (file-flags+ flags (file-flags write-only)))
         (maybe-mode (if (null? rest) '() (cdr rest))))
    (apply open-file fname flags maybe-mode)))

;;; Port/fd ops
;;; ----------------------------------

; Not Exported ;
;; Given fdes, return a port with (revealed count++)
; Lookup by port by fdes, incrementing revealed count
;  OR make a new port with revealed count of 1 
(define (fdes->port fd port-maker)  
  (cond  ((maybe-ref-fdport-port fd) =>
          (lambda (p)
            (increment-revealed-count p 1)
            p))
         (else (port-maker fd 1 
                (string-append "<fdes->port on fd " (number->string fd) ">")))))

;; Gets a port mapped to given fd + increments the revealed count
(define (fdes->inport fd)
  (let ((port (fdes->port fd make-input-fdport/fd)))
    (if (not (input-port? port))
        (error "fdes was already assigned to an outport" fd)
        port)))

;; Gets a port mapped to given fd + increments the revealed count
(define (fdes->outport fd)
  (let ((port (fdes->port fd make-output-fdport/fd)))
    (if (not (output-port? port))
        (error "fdes was already assigned to an inport" fd)
        port)))

;; Gets an fd associated with port + increments the revealed count
(define (port->fdes port)
  (check-arg open-fdport? port port->fdes)
  (increment-revealed-count port 1)
  (fdport->fd port))

;; Gets the port revealed count
(define (port-revealed port)
  (check-arg fdport? port port-revealed)
  (let ((count (fdport:revealed port)))
    (and (not (zero? count)) count)))

;; Decrements port revealed count
(define (release-port-handle port)
  (check-arg fdport? port release-port-handle)
  (atomically!
   (let ((rev (fdport:revealed port)))
     (if (not (zero? rev))
         (let ((new-rev (- rev 1)))
           (set-fdport:revealed! port new-rev))))))

;; Calls consumer on a file descriptor; takes care of revealed bookkeeping. 
; If fd/port is a file descriptor, this is just (consumer fd/port). 
; If fd/port is a port, calls consumer on its underlying file descriptor. 
; While consumer is running, the port's revealed count is incremented.
(define (call/fdes fd/port consumer)
  (cond ((integer? fd/port)
         (consumer fd/port))
        ((fdport? fd/port)
         (let ((port fd/port))
           (dynamic-wind
            (lambda ()
              (if (not port) (error "Can't throw back into call/fdes.")))
            (lambda () (consumer (port->fdes port))) ; increments revealed count
            (lambda ()
              (release-port-handle port)
              (set! port #f)))))
        (else (error "Not a file descriptor or fdport." fd/port))))

; Not Exported ;
;; Don't mess with the revealed count in the port case
; -- just sneakily grab the fdes and run.
(define (sleazy-call/fdes fd/port proc)
  (proc (cond ((integer? fd/port) fd/port)
              ((fdport? fd/port)  (fdport->fd fd/port))
              (else (error "Not a file descriptor or fdport." fd/port)))))

;; Moves an i/o handle FD/PORT to fd TARGET.
; - If FD/PORT is a file descriptor, this is dup2(); close().
; - If FD/PORT is a port, this shifts the port's underlying file descriptor
;   to TARGET, as above, closing the old one. Port's revealed count is
;   set to 1.
; TARGET is evicted before the shift -- if there is a port allocated to
; file descriptor TARGET, it will be shifted to another file descriptor.
(define (move->fdes fd/port target)
  (check-arg fd/port? fd/port dup->fdes)
  (evict-ports target)
  (cond ((and (integer? fd/port) (not (= fd/port target)))
          (%dup2 fd/port target)
          (close-fdes fd/port))
        ((fdport? fd/port)
          (move-fdport! fd/port target)))) ; Takes care of fd==target case

;;; If FD/PORT is a file descriptor, returns a file descriptor.
;;; If FD/PORT is a port, returns a port.
(define (dup fd/port . maybe-target)
  (check-arg fd/port? fd/port dup)
  (apply (cond 
          ((integer?     fd/port) dup->fdes)
	        ((input-port?  fd/port) dup->inport)
	        ((output-port? fd/port) dup->outport))
	 fd/port maybe-target))

(define (dup->fdes fd/port . maybe-target)
  (check-arg fd/port? fd/port dup->fdes)
  (if (pair? maybe-target)
      (let ((target (car maybe-target)))
	      (evict-ports target)	; Evict port open on target
        ; If target is fd, it will be closed silently by dup2
	      (sleazy-call/fdes fd/port (lambda (fd) (%dup2 fd target))))
      (sleazy-call/fdes fd/port %dup)))

(define (dup->inport fd/port . maybe-target)
  (apply really-dup->port make-input-fdport/fd fd/port maybe-target))

(define (dup->outport fd/port . maybe-target)
  (apply really-dup->port make-output-fdport/fd fd/port maybe-target))

(define (really-dup->port port-maker fd/port . maybe-target)
  (let ((fd (apply dup->fdes fd/port maybe-target))
        (revealed-count (if (null? maybe-target) 0 1))
        (os-path (if (fdport? fd/port) 
                     (fdport->os-path fd/port) 
                     "<dup'd from fd>")))
    (port-maker fd revealed-count os-path)))

;;; Not exported.
(define (shell-open path flags fdes)
  (%dup2 (port->fdes (open-file (stringify path) flags (file-mode read write))) fdes))

(define create+trunc
  (file-flags write-only create truncate))

(define write+append+create
  (file-flags write-only append create))

(define read-only
  (file-flags read-only))

;;; Generic port operations
;;; -----------------------

;; (close-after port f)
;;   Apply F to PORT. When F returns, close PORT, then return F's result.
;;   Does nothing special if you throw out or throw in.
(define (close-after port f)
  (receive vals (f port)
    (close port)
    (apply values vals)))

(define (close port/fd)
  ((cond ((integer? port/fd)     close-fdes)
         ((output-port? port/fd) close-output-port)
         ((input-port?  port/fd) close-input-port)
         (else (error "Not file-descriptor or port" port/fd)))  port/fd))

; Not Exported ;
;; Moves the given fdport from it's current fd 
; If target not specified, moves with dup; otherwise, dup2
; Takes care of all the internal bookkeeping, accounts for fd==target case
(define (move-fdport! fdport . maybe-target)
  (call-with-current-continuation
    (lambda (ret)
      (let* ((old-fd (fdport->fd fdport))
             (target (:optional maybe-target #f))
             (revealed-count (if target 1 0)))
        (if (eq? old-fd target) (ret))
        (if target ; reset-fdport-channel will close old channels
            (reset-fdport-channel/fd fdport (%dup2 old-fd target))
            (reset-fdport-channel/fd fdport (%dup old-fd)))
        (delete-fdport! old-fd)
        (set-fdport! (fdport->fd fdport) fdport revealed-count)))))  

;; If this fd has an associated input or output port,
;; move it to a new fd, freeing this one up.
(define (evict-ports fd)
  (cond ((maybe-ref-fdport-port fd) =>    ; Does not bump the revealed count.
         (lambda (fdport)                 ; Even if the revealed count!=0, we shift it silently 
          (move-fdport! fdport)
          #t))
        (else #f)))

(define (close-fdes fd)
  (if (evict-ports fd)
      #t ; EBADF should not occur if there is a port
      (%close-fdes fd)))

(define (force-output . maybe-i/o) 
  (let ((stream (:optional maybe-i/o (current-output-port))))
    (if (output-port? stream) (s48-force-output stream)
        (values))))

(define (flush-fdport fdport)
  (force-output (check-arg fdport? fdport flush-fdport)))

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

(define (pipe)
  (apply (lambda (r-fd w-fd)
           (let ((r (make-input-fdport/fd  r-fd 0 "<read end of pipe>"))
                 (w (make-output-fdport/fd w-fd 0 "<write end of pipe>")))
             (values r w)))
         (%pipe-fdes)))

;;; Some of fcntl()
;;;;;;;;;;;;;;;;;;;

;;; fcntl()'s F_GETFD and F_SETFD.  Note that the SLEAZY- prefix on the
;;; CALL/FDES isn't an optimisation; it's *required* for the correct behaviour
;;; of these procedures. Straight CALL/FDES modifies unrevealed file
;;; descriptors by clearing their CLOEXEC bit when it reveals them -- so it
;;; would interfere with the reading and writing of that bit!


(define (close-on-exec? fd/port)
  (sleazy-call/fdes fd/port
    (lambda (fd) (%get-cloexec fd))))
(define fdes-flags close-on-exec?)
    
(define (set-close-on-exec?! fd/port cloexec?)
  (sleazy-call/fdes fd/port
    (lambda (fd) (%set-cloexec fd cloexec?))))
(define set-fdes-flags set-close-on-exec?!)

;;; fcntl()'s F_GETFL and F_SETFL.
;;; Get: Returns open flags + get-status flags (below)
;;; Set: append, sync, async, nbio, nonblocking, no-delay

(define (file-status-flags fd/port)
  (sleazy-call/fdes fd/port
    (lambda (fd) (%get-stat-flag fd))))
(define fdes-status file-status-flags)
    
(define (set-file-status-flags! fd/port flags)
  (sleazy-call/fdes fd/port
    (lambda (fd) (%set-stat-flag fd flags))))
(define set-fdes-status set-file-status-flags!)
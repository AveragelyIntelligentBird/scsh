;;; Part of scsh 1.0. See file COPYING for notices and license.
;; Contains general scsh fdport makers, port-reveal getter/setters, port flushing 
;; and stdio fdport system init.

;;; A functional search tree mapping integer file descriptors to ports. 
;; It is in a cell so that reffing and setting can be done provisionally
;; and be protected by optimistic concurrency.
(define *fdports* (make-cell (make-search-tree = <)))

;;; Sets the port and reveal count for fd.
;; We always replace the entry if fd was already in the table.
(define (set-fdport! fd port revealed)
  (atomically!
   (delete-fdport! fd)
   (let ((ports-table (provisional-cell-ref *fdports*)))
     (if (not (zero? revealed))
         (begin (provisional-cell-set! *fdports* (search-tree-insert ports-table fd (cons port revealed)))
                (%set-cloexec fd #f))
         (begin
           (provisional-cell-set! *fdports* (search-tree-insert ports-table fd (cons (make-weak-pointer port) revealed)))
           (%set-cloexec fd #t))))))

;;; Removes fd from the table if it was installed.
(define (delete-fdport! fd)
  (atomically!
   (provisional-cell-set!
    *fdports* (search-tree-delete (provisional-cell-ref *fdports*) fd))))

;;; Returns the port and revealed count for fd in a cons cell (port . revealed).
;; Returns #f if fd wasn't installed.
(define (maybe-ref-fdport fd)
  (atomically
   (let* ((ports-table (provisional-cell-ref *fdports*))
          (ref (search-tree-ref ports-table fd)))
     (and ref (if (weak-pointer? (car ref))
                  (let ((val (weak-pointer-ref (car ref))))
                    (if val
                        (cons val (cdr ref))
                        (begin (provisional-cell-set! *fdports* (search-tree-delete ports-table fd))
                               #f)))
                  ref)))))

;;; Uses reffer to get a desired value from the cons pair returned by (maybe-ref-fdport fd)
(define (maybe-ref-fdport-* reffer fd)
  (let ((ref (maybe-ref-fdport fd)))
    (if ref
        (reffer ref)
        ref)))

;;; Returns the port mapped to fd, or #f if it wasn't installed.
(define (maybe-ref-fdport-port fd)
  (maybe-ref-fdport-* car fd))

;;; Returns fd's revealed count, or #f if it wasn't installed.
(define (maybe-ref-fdport-revealed fd)
  (maybe-ref-fdport-* cdr fd))

;;; Closer for fdports
(define (close-fdport-channel channel)
  (delete-fdport! (channel-os-index channel))
  (close-channel channel))

;;; Finalizer for fdports (runs before being GC'd)
(define (finalize-fdport port)
  (force-output-if-open port) ; Does nothing on input ports
  (close-fdport-channel (fdport->channel port)))

;;; Portmakers
;;; ----------------------------------

;;; Core portmakers. Construct scsh ports for a given bufpolicy.
;; We still use s48 port record because it is integrated in the s48 VM as a stob
;; fdport extentions are supported via port-data channel-cell as described in
;; fdport-internal.scm
(define (make-input-fdport channel bufpol . maybe-buffer-size)
  (let* ((buffer-size (if (null? maybe-buffer-size) 
                          max-soft-bufsize 
                          (car maybe-buffer-size)))
         (bufpol (if (= 1 buffer-size) bufpol/none bufpol))) 
  (cond 
      ; Errors
      ((or (not (integer? buffer-size)) (>= 0 buffer-size)) 
          (assertion-violation 'make-input-fdport
            "invalid buffer size"
            make-input-fdport channel buffer-size))
      ((not (channel? channel))
          (assertion-violation 'make-input-fdport
            "invalid channel"
            make-input-fdport channel buffer-size))
      ((buf-policy=? bufpol bufpol/line)
        (assertion-violation 'make-input-fdport
          "line buffering is invalid on input ports"
          make-input-fdport channel bufpol))
      ; Valid ports
      (else  ; TODO: do we want to warn about too small buf for block?
       (really-make-input-fdport channel bufpol buffer-size close-fdport-channel finalize-fdport)))))      
          
(define (make-output-fdport channel bufpol . maybe-buffer-size)
  (let* ((buffer-size (if (null? maybe-buffer-size) 
                          max-soft-bufsize 
                         (car maybe-buffer-size)))
         (bufpol (if (= 0 buffer-size) bufpol/none bufpol))) 
  (cond 
      ; Errors
      ((or (not (integer? buffer-size)) (> 0 buffer-size))
          (assertion-violation 'make-output-fdport
            "invalid buffer size"
            make-output-fdport channel buffer-size))
      ((not (channel? channel))
          (assertion-violation 'make-output-fdport
            "invalid channel"
            make-output-fdport channel buffer-size))
      ; Valid ports
      (else ; TODO: do we want to warn about too small buf for block?
        (really-make-output-fdport channel bufpol buffer-size close-fdport-channel finalize-fdport)))))

;;; Makers for fdports given an fd
(define (make-input-fdport/fd fd revealed os-path)
  (let* ((input-fdchannel (open-channel fd os-path (enum channel-status-option input) #t))
         (port (make-input-fdport input-fdchannel bufpol/block)))
    (set-fdport! fd port revealed)
    port))

(define (make-output-fdport/fd fd revealed os-path)
  (let* ((output-fdchannel (open-channel fd os-path (enum channel-status-option output) #t))
         (port (make-output-fdport output-fdchannel bufpol/block)))
    (set-fdport! fd port revealed)
    port))

(define (remake-stdio-fdport fd s48-port input?)
  (let* ((stdio-channel (s48-port->channel s48-port))
         (port (if input?
                  (make-input-fdport  stdio-channel bufpol/block)
                  (make-output-fdport stdio-channel bufpol/block))))
    (set-fdport! fd port 1) ; Setting port-revealed count to 1 from startup
    port))

;;; Predicates and arg checkers
;;; ----------------------------------

;;; Checks if x is a scsh fdport port, and is installed in *fdports*
(define (fdport? x)
  (and (or (input-port? x) (output-port? x))
       (maybe-ref-fdport (fdport->fd x))
       #t)) ; and returns last non-false val

;;; Checks if x is an fdport and it is currently open
(define (open-fdport? x)
  (and (fdport? x) 
    (or (open-output-port? x) 
        (open-input-port? x))))

;;; Checks if x is EITHER an FD integer or an fdport 
(define (fd/port? x)
  (or (and (integer? x) (>= x 0))
      (fdport? x)))

;;; Port-reveal getters and setters
;;; ------------------------

;;; Getter for revealed count
(define (fdport:revealed fdport)
  (check-arg fdport? fdport fdport:revealed)
  (maybe-ref-fdport-revealed (fdport->fd fdport)))

;;; Setter for revealied count
(define (set-fdport:revealed! fdport revealed)
  (check-arg fdport? fdport set-fdport:revealed!)
  (set-fdport! (fdport->fd fdport) fdport revealed))

;;; Reconstructs port entry in *fdports* with updated revealed count
(define (increment-revealed-count port delta)
  (atomically!
   (let* ((count (fdport:revealed port))
          (newcount (+ count delta)))
     (set-fdport:revealed! port newcount))))

(define (fdport-channel-ready? fdport) ; TODO - do we need this?
  (channel-ready? (fdport->channel fdport)))

;;; Fdport flush control
;;; ---------------------

;;; Flushes all output fdports
(define (flush-all-ports)
  (let ((thunks (output-fdport-forcers #f))) 
    (cond ((null? thunks)
           #f)
          (else
           (let loop ((threads
                       (map spawn-thread thunks))
                      (new-threads '()))
             (cond
              ((not (null? threads))
               (if (thread-continuation
                    (car threads))
                   (loop (cdr threads)
                         (cons (car threads) new-threads))
                   (loop (cdr threads) new-threads)))
              ((not (null? new-threads))
               (loop new-threads '()))))
           #t))))

(define (spawn-thread thunk)
  (let ((placeholder (make-placeholder)))
    (spawn
     (lambda ()
       (placeholder-set!
        placeholder
        (current-thread))
       (thunk)))
    (placeholder-value placeholder)))

;;; Bare-bones flush; used at system exit
(define (flush-all-ports-no-threads)
  (let ((thunks (output-fdport-forcers #f))) 
    (for-each (lambda (thunk) (thunk)) thunks)))

;;; System initialization
;;; ---------------------

;;; JMG: should be deprecated-proc
(define error-output-port
  current-error-port)

(define (initialize-fdport-i/o* thunk)
  (let ((in  (remake-stdio-fdport 0 (current-input-port) #t))
        (out (remake-stdio-fdport 1 (current-output-port) #f))
        (err (remake-stdio-fdport 2 (current-error-port) #f)))
    (initialize-i/o in out err thunk)))

(define-simple-syntax (initialize-fdport-i/o body ...)
  (initialize-fdport-i/o* (lambda () body ...)))

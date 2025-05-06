;;; Part of scsh 1.0. See file COPYING for notices and license.
;; fdport makers + lookup table management + special ports (stdio) + port group management (init, flush all)

;;; Scsh portmakers. Construct scsh ports for a given bufpolicy
;; We still use s48 port record because it is integrated in the s48 VM as a stob
;; Any extensions, such as port reveal count and bufpol, are managed by scsh's lookup
;; table, as can be seen in newports.scm

;; Actual fdport makers

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
      ((buf-policy=? bufpol bufpol/block) ; TODO: do we want to warn about too small buf for block?
        (debug-message "Making block buf input port")
        (really-make-input-fdport channel bufpol buffer-size close-fdport-channel))
      ((buf-policy=? bufpol bufpol/none)
        (debug-message "Making unbuf input port")
        (really-make-input-fdport channel bufpol buffer-size close-fdport-channel)))))
          
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
      ((buf-policy=? bufpol bufpol/block) ; TODO: do we want to warn about too small buf for block?
        (debug-message "Making block buf output port")
        (really-make-output-fdport channel bufpol buffer-size close-fdport-channel))
      ((buf-policy=? bufpol bufpol/line)
        (debug-message "Making line buf output port")
        (really-make-output-fdport channel bufpol buffer-size close-fdport-channel))
      ((buf-policy=? bufpol bufpol/none)
        (debug-message "Making unbuf output port")
        (really-make-output-fdport channel bufpol buffer-size close-fdport-channel)))))



;;; A functional search tree mapping integer file descriptors to ports. I'm
;;; putting it all in a cell so that reffing and setting can be done provisionally
;;; and be protected by optimistic concurrency.
(define *fdports* (make-cell (make-search-tree = <)))

;;; Sets the port and reveal count for fd, and always replaces if fd was already
;;; in the table.
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
;;; Returns #f if fd wasn't installed.
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

;;; Uses reffer to get a desired value from the cons pair returned by
;;; (maybe-ref-fdport fd)
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

; TODO change the name to explicitly include the fd
(define (make-input-channel fd)
  (open-channel fd "input" (enum channel-status-option input) #t))

(define (make-output-channel fd)
  (open-channel fd "output" (enum channel-status-option output) #t))

(define (close-fdport-channel channel)
  (delete-fdport! (channel-os-index channel))
  (close-channel channel))

(define (make-input-fdport/fd fd revealed)
  (let ((port (make-input-fdport (make-input-channel fd))))
    (set-fdport! fd port revealed)
    port))

(define (make-output-fdport/fd fd revealed)
  (let ((port (make-output-fdport (make-output-channel fd) bufpol/block)))
    (set-fdport! fd port revealed)
    port))

;;; This is now really just a check if x is a channel port, and is
;;; installed in *fdports*
(define (fdport? x)
  (and (or (input-port? x) (output-port? x))
       (fdport->channel x)  ; Has OS-channel in the data field
       (maybe-ref-fdport (fdport->fd x))
       #t))

(define (fdport:revealed fdport)
  (check-arg fdport? fdport fdport:revealed)
  (maybe-ref-fdport-revealed (fdport->fd fdport)))

(define (set-fdport:revealed! fdport revealed)
  (check-arg fdport? fdport set-fdport:revealed!)
  (set-fdport! (fdport->fd fdport) fdport revealed))

(define (fdport-channel-ready? fdport)
  (channel-ready? (fdport->channel fdport)))
(define (increment-revealed-count port delta)
  (atomically!
   (let* ((count (fdport:revealed port))
          (newcount (+ count delta)))
     (set-fdport:revealed! port newcount))))




  
;;


(define (flush-all-ports)
  (let ((thunks (output-fdport-forcers #f))) ; TODO - is it a good idea to only do fdports?
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

(define (flush-all-ports-no-threads)
  (let ((thunks (append (output-fdport-forcers #f) (output-port-forcers #f)))) ; Flushes fdports and s48 ports (stdio) - TODO, revise all to be fdports?
    (for-each (lambda (thunk) (thunk)) thunks)))



;;; Initialise the system
;;; ---------------------

;;; JMG: should be deprecated-proc
(define error-output-port
  current-error-port)

; TODO replace with fdports
(define (init-fdports!)
  (set-fdport! (s48-port->fd (current-input-port))  (current-input-port)  1)
  (set-fdport! (s48-port->fd (current-output-port)) (current-output-port) 1)
  (set-fdport! (s48-port->fd (current-error-port))  (current-error-port)  1))



;;; Random predicates and arg checkers
;;; ----------------------------------

(define (open-fdport? x)
  (and (fdport? x) (or (open-output-port? x) (open-input-port? x))))

(define (fdport-open? port)
  (check-arg fdport? port fdport-open?)
  (eq? (channel-status (fdport->channel port))
       (enum channel-status-option closed)))


(define (fd/port? x)
  (or (and (integer? x) (>= x 0))
      (output-port? x)
      (input-port? x)))
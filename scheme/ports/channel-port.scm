; Since s48 did not expose some useful channel-port forms via its top-level interface, 
; we copy them over as is. Our goal is to keep scsh a self-contained s48 package, and 
; and this seems to be the best way. 
; Below is a slightly extended version of s48's rst/channel-port.scm
;========================================================================================

; Part of Scheme 48 1.9.  See file COPYING for notices and license.
; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber
; Ports built on OS channels.

;----------------
; Records used as the PORT-DATA value in ports that read or write to channel.
; CLOSER is a function that closes the channel; socket channels have their own
; closing method.

(define-synchronized-record-type channel-cell :channel-cell
  (really-make-channel-cell channel closer condvar in-use?)
  (in-use? sent)
  channel-cell?
  (channel channel-cell-ref)
  (closer  channel-cell-closer)
  (condvar channel-cell-condvar)
  (in-use? channel-cell-in-use? set-channel-cell-in-use?!)
  (sent    channel-cell-sent    set-channel-cell-sent!))

(define (make-channel-cell channel closer)
  (really-make-channel-cell channel closer (make-condvar) #f))

; TODO: getter and setter for bufpol
; TODO: transfer all lookup table stuff here
; TODO add bufpol into the struct

; Extracting the channel from a port.

(define (fdport->channel port)
  (let ((data (port-data port)))
    (if (channel-cell? data)
	(channel-cell-ref data)
	#f)))

(define (fdport->fd port)
  (let ((channel (fdport->channel port)))
    (if channel
				(channel-os-index channel)
				#f)))

; Closing a port's channel.  This is called with a proposal already in place.

(define (port-channel-closer cell)
  (channel-maybe-commit-and-close (channel-cell-ref cell)
				  (channel-cell-closer cell)))

(define (channel-port-ready? port)
  (let ((ready? (channel-ready? (channel-cell-ref (port-data port)))))
    (if (maybe-commit)
				(values #t ready?)
				(values #f #f))))

;----------------
; Block-buffered Input Ports

; Four possibilities:
;   A. there is no read in progress
;       -> initiate a read
;   B. a read has completed
;       -> update the port
;   C. a read has been started and has not completed
;       -> wait for it
;   D. we don't want to wait
;       -> so we don't

(define (fill-buffer! port wait?)
  (let ((cell (port-data port))
	(buffer (port-buffer port)))
    (let ((condvar (channel-cell-condvar cell))
	  (channel (channel-cell-ref cell)))
      (cond ((not (channel-cell-in-use? cell))
	     (set-channel-cell-in-use?! cell #t)
	     (let ((limit (provisional-port-limit port)))
	       (channel-maybe-commit-and-read channel
					      buffer
					      limit
					      (- (byte-vector-length buffer) limit)
					      condvar
					      wait?))
	     #f)	; caller should retry as results may now be available
	    ((condvar-has-value? condvar)
	     (let ((result (condvar-value condvar)))
	       (set-channel-cell-in-use?! cell #f)
	       (set-condvar-has-value?! condvar #f)
	       (note-buffer-reuse! port)
	       (cond
		((eof-object? result)
		 (provisional-set-port-pending-eof?! port #t))
		((i/o-error? result)
		 (if (maybe-commit)
		     (signal-condition result)
		     #f))
		(else
		 (provisional-set-port-limit! port
					      (+ (provisional-port-limit port) result))))
	       (maybe-commit)))
	    (wait?
	     (maybe-commit-and-wait-for-condvar condvar #f))
	    (else
	     (maybe-commit))))))

(define buf-input-fdport-handler
  (make-buffered-input-port-handler
     (lambda (cell)
       (list 'buf-input-fdport (channel-cell-ref cell)))
     port-channel-closer
     fill-buffer!
     channel-port-ready?))

; Assuming all arguments are valid 
(define (make-buf-input-fdport channel buffer-size closer)
	(let ((port
	       (make-buffered-input-port 
						buf-input-fdport-handler
						(make-channel-cell channel closer)
						(make-byte-vector buffer-size 0)
						0
						0)))
	(set-port-crlf?! port (channel-crlf?))
	(set-port-text-codec! port utf-8-codec) ; Accounts for s48 bug - new ports are latin-1
	port))

;----------
; Unbuffered Input ports

; An unbuffered read reads (or attempts to read) excatly the number of bytes
; requested in a single syscall. This behavior is straighforward for reading byte
; streams, but it gets significantly more involved if we want to read variable-length
; char encodings one character at a time. To approximate the behavior, we will read one 
; byte at a time until we either can decode a character OR we have exhausted max char length 
; for a given encoding.  

; One-byte-at-a-time "unbuffered" buffer filler
; TODO - possibly abstract with fill-buffer!
(define (fill-one-byte-buffer! port wait?)
  (let ((cell (port-data port))
				(buffer (port-buffer port)))
    (let ((condvar (channel-cell-condvar cell))
	  			(channel (channel-cell-ref cell)))
      (cond 
				((not (channel-cell-in-use? cell))
					(set-channel-cell-in-use?! cell #t)
					(let ((limit (provisional-port-limit port)))
						(channel-maybe-commit-and-read channel
										buffer
										limit
										1  ; Read up to one byte
										condvar
										wait?))
					#f)	; caller should retry as results may now be available
				((condvar-has-value? condvar)
				 (let ((result (condvar-value condvar)))
					(set-channel-cell-in-use?! cell #f)
					(set-condvar-has-value?! condvar #f)
					(note-buffer-reuse! port)
					(cond
						((eof-object? result)
							(provisional-set-port-pending-eof?! port #t))
						((i/o-error? result)
							(if (maybe-commit)
									(signal-condition result)
									#f))
						(else
							(provisional-set-port-limit! port
													(+ (provisional-port-limit port) result))))
					(maybe-commit)))
				(wait?
					(maybe-commit-and-wait-for-condvar condvar #f))
				(else
					(maybe-commit))))))

(define unbuf-input-fdport-handler
  (let* ((handler 
						(make-buffered-input-port-handler
							(lambda (cell)
								(list 'unbuf-input-fdport (channel-cell-ref cell)))
							port-channel-closer
							fill-one-byte-buffer!
							channel-port-ready?)))
		; Remaking the port because s48 does not define a setter for block handler :<
		(make-port-handler 
      (port-handler-discloser handler)
      (port-handler-close handler) 
      (port-handler-byte handler)
      (port-handler-char handler)
      ; TODO: Redefine block handler to be a direct read into given buffer
			#f
      (port-handler-ready? handler)
      (port-handler-force handler))))

(define unbuf-port-buf-size 64) ; Arbitrary small vlaue for internal buffers

(define (make-unbuf-input-fdport channel closer)
	(let ((port
	       (make-buffered-input-port 
						buf-input-fdport-handler
						(make-channel-cell channel closer)
						(make-byte-vector unbuf-port-buf-size 0) 
						0
						0)))
	(set-port-crlf?! port (channel-crlf?))
	(set-port-text-codec! port utf-8-codec) ; Accounts for s48 bug - new ports are latin-1
	port))

;----------------
; Output ports

; A. No write already in progress
;     -> start one
; B. A write has completed
;     -> if we're done then reset the index, otherwise write some more
; C. Wait.
;
; If NECESSARY? is #f we are doing a periodic buffer flushing and shouldn't
; bother to wait if someone else is already writing out the buffer.

(define (empty-buffer! port necessary?)
  (let* ((cell (port-data port))
	 (condvar (channel-cell-condvar cell)))

    (cond ((not (channel-cell-in-use? cell))
	   (let ((buffer (port-buffer port))
		 (count (provisional-port-index port)))
	     (set-channel-cell-in-use?! cell #t)
	     (send-some port 0 necessary?)))
	  ((condvar-has-value? condvar)
	   (let ((result (condvar-value condvar)))
	     (set-condvar-has-value?! condvar #f)
	     (if (i/o-error? result)
		 (begin
		   ;; #### We should probably maintain some kind of
		   ;; "error status" with the channel cell that allows
		   ;; actual recovery.
		   ;; The way it is, we just pretend we're done so the
		   ;; the periodic buffer flushing doesn't annoy the heck
		   ;; out of us.
		   (provisional-set-port-index! port 0)
		   ;; good housekeeping; also keeps port-buffer flusher sane
		   (provisional-set-port-pending-eof?! port #f)
		   (note-buffer-reuse! port)
		   (set-channel-cell-in-use?! cell #f)
		   (if (maybe-commit)
		       (signal-condition result)
		       #f))
		 (let ((sent (+ result (channel-cell-sent cell))))
		   (if (< sent
			  (provisional-port-index port)) 
		       (send-some port sent necessary?)
		       (begin
			 (provisional-set-port-index! port 0)
			 (note-buffer-reuse! port)
			 (set-channel-cell-in-use?! cell #f)
			 (maybe-commit)))))))
	  (necessary?
	   (maybe-commit-and-wait-for-condvar condvar #f))
	  (else
	   (maybe-commit)))))

; Try writing the rest of PORT's buffer. SENT bytes have already been
; written out.

(define (send-some port sent wait?)
  (let ((cell (port-data port)))
    (set-channel-cell-sent! cell sent)
    (channel-maybe-commit-and-write (channel-cell-ref cell)
				    (port-buffer port)
				    sent
				    (- (provisional-port-index port)
				       sent)
				    (channel-cell-condvar cell)
				    wait?)))

(define output-channel-handler
  (make-buffered-output-port-handler
     (lambda (cell)
       (list 'output-port
	     (channel-cell-ref cell)))
     port-channel-closer
     empty-buffer!
     channel-port-ready?))

(define (output-channel->port channel . maybe-buffer-size)
  (let ((port
	 (if (and (not (null? maybe-buffer-size))
		  (eq? 0 (car maybe-buffer-size)))
	     (make-unbuffered-output-port unbuffered-output-handler
					  (make-channel-cell channel close-channel))
	     (real-output-channel->port channel maybe-buffer-size close-channel))))
    (set-port-crlf?! port (channel-crlf?))
    port))

; This is for sockets, which have their own closing mechanism.

(define (output-channel+closer->port channel closer . maybe-buffer-size)
  (real-output-channel->port channel maybe-buffer-size closer))
	     
; Dispatch on the buffer size to make the appropriate port.  A buffer
; size of zero creates an unbuffered port.  Buffered output ports get a
; finalizer to flush the buffer if the port is GC'ed.

(define (real-output-channel->port channel maybe-buffer-size closer)
  (let ((buffer-size (if (null? maybe-buffer-size)
			 (channel-buffer-size)
			 (car maybe-buffer-size))))
    (if  (or (not (integer? buffer-size))
	     (< buffer-size 0)
	     (not (channel? channel)))
	 (assertion-violation 'real-output-channel->port
			      "invalid argument"
			      output-channel->port channel buffer-size)
	 (let ((port (make-buffered-output-port output-channel-handler
						(make-channel-cell channel
								   closer)
						(make-byte-vector buffer-size 0)
						0
						buffer-size)))
	   (periodically-force-output! port)
	   (add-finalizer! port force-output-if-open)
	   port))))
	     
;----------------
; Various ways to open ports on files.

; First a generic procedure to do the work.

(define (maybe-open-file op file-name option close-silently? coercion)
  (let ((thing
	 (with-handler
	  (lambda (c punt)
	    (cond
	     ((and (vm-exception? c)
		   (eq? 'os-error
			(vm-exception-reason c)))
	      (punt (condition
		     (make-i/o-error)
		     (make-who-condition op)
		     (make-message-condition
		      (os-string->string
		       (byte-vector->os-string
			(os-error-message (car (reverse (condition-irritants c)))))))
		     (make-irritants-condition (list file-name)))))
	     (else
	      (punt))))
	  (lambda ()
	    (let ((file-name/os (x->os-string file-name)))
	      (open-channel (os-string->byte-vector file-name/os)
			    (os-string->string file-name/os)
			    option close-silently?))))))
    (coercion thing (channel-buffer-size))))
  
; And then all of RnRS's file opening procedures.

; (define (really-open-input-file op string close-silently?)
;   (maybe-open-file op
; 		   string
; 		   (enum channel-status-option input)
; 		   close-silently?
; 		   input-channel->port))

; (define (open-input-file string)
;   (really-open-input-file 'open-input-file string  #f))

(define (really-open-output-file op string close-silently?)
  (maybe-open-file op
		   string
		   (enum channel-status-option output)
		   close-silently?
		   output-channel->port))

(define (open-output-file string)
  (really-open-output-file 'open-output-file string #f))

; (define (call-with-input-file string proc)
;   (let* ((port (really-open-input-file 'call-with-input-file string #t))
;          (results (call-with-values (lambda () (proc port))
; 				    list)))
;     (close-input-port port)
;     (apply values results)))

(define (call-with-output-file string proc)
  (let* ((port (really-open-output-file 'call-with-output-file string #t))
         (results (call-with-values (lambda () (proc port))
				    list)))
    (close-output-port port)
    (apply values results)))

; (define (with-input-from-file string thunk)
;   (call-with-input-file string
;     (lambda (port)
;       (call-with-current-input-port port thunk))))

(define (with-output-to-file string thunk)
  (call-with-output-file string
    (lambda (port)
      (call-with-current-output-port port thunk))))

;----------------
; Flush the output buffers of all channel output ports.  This is done before
; forking the current process.

(define (force-channel-output-ports!)
  (for-each (lambda (port)
	      (if (fdport->channel port)
		  (force-output-if-open port)))
	    (periodically-flushed-ports)))

;----------------
; Unbuffered output channel ports.
; This is used for the initial current-error-port.

(define unbuffered-output-handler
  (make-unbuffered-output-port-handler (lambda (port)
					 (list 'output-port
					       (channel-cell-ref (port-data port))))
				       (lambda (port)
					 (port-channel-closer (port-data port)))
				       (lambda (port buffer start count)
					 (channel-write (channel-cell-ref (port-data port))
							buffer start count))
				       (lambda (port)			; ready
					 (channel-ready? (channel-cell-ref (port-data port))))))

; Utilities

(define (channel-buffer-size)
  (channel-parameter (enum channel-parameter-option buffer-size)))

(define (channel-crlf?)
  (channel-parameter (enum channel-parameter-option crlf?)))

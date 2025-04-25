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

(define (make-buffer-filler unbuf?)
  (lambda (port wait?) 
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
								(if unbuf? 1 (- (byte-vector-length buffer) limit))
								condvar
								wait?))
			; (debug-message "READ")  ; debugging
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
			(maybe-commit)))))))

(define buf-input-fdport-handler
  (make-buffered-input-port-handler
     (lambda (cell)
       (list 'blockbuf-input-fdport (channel-cell-ref cell)))
     port-channel-closer
     (make-buffer-filler #f)
     channel-port-ready?))

; Assuming all arguments are valid 
(define (make-blockbuf-input-fdport channel buffer-size closer)
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

(define unbuf-input-fdport-handler/block-handler
  (let* ((handler 
			(make-buffered-input-port-handler
				(lambda (cell)
					(list 'unbuf-input-fdport (channel-cell-ref cell)))
				port-channel-closer
				(make-buffer-filler #t)
				channel-port-ready?)))
		; Remaking the port because s48 does not define a setter for block handler :<
		(make-port-handler 
			(port-handler-discloser handler)
			(port-handler-close handler) 
			(port-handler-byte handler)
			(port-handler-char handler)
			#f ; TODO: do we want to add a direct read into buffer here? or do we want to loop individual byte reads?
			(port-handler-ready? handler)
			(port-handler-force handler))))

(define unbuf-input-fdport-handler
  (make-buffered-input-port-handler
	(lambda (cell)
		(list 'unbuf-input-fdport (channel-cell-ref cell)))
	port-channel-closer
	(make-buffer-filler #t)
	channel-port-ready?))

(define unbuf-port-buf-size 64) ; Arbitrary small vlaue for internal buffers

(define (make-unbuf-input-fdport channel closer)
	(let ((port
	       (make-buffered-input-port 
						unbuf-input-fdport-handler
						(make-channel-cell channel closer)
						(make-byte-vector unbuf-port-buf-size 0) 
						0
						0)))
	(set-port-crlf?! port (channel-crlf?))
	(set-port-text-codec! port utf-8-codec) ; Accounts for s48 bug - new ports are latin-1
	port))

;----------------
; Buffered Output ports

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
    (cond 
	 ((not (channel-cell-in-use? cell))
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
				(if (< sent (provisional-port-index port)) 
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
				    (- (provisional-port-index port) sent)
				    (channel-cell-condvar cell)
				    wait?)))

(define blockbuf-output-fdport-handler
  (make-buffered-output-port-handler
     (lambda (cell)
       (list 'blockbuf-output-fdport (channel-cell-ref cell)))
     port-channel-closer
     empty-buffer!
     channel-port-ready?))

(define (make-blockbuf-output-fdport channel buffer-size closer)
	(let ((port
	       (make-buffered-output-port 
				blockbuf-output-fdport-handler
				(make-channel-cell channel closer)
				(make-byte-vector buffer-size 0)
				0
				buffer-size)))
	; (periodically-force-output! port) ; TODO do we want to enable this at all in scsh? maybe bufpol/auto?
	(add-finalizer! port force-output-if-open) ; s48 forces output if it gc's a an open output port; do we want this?
	(set-port-crlf?! port (channel-crlf?))
	(set-port-text-codec! port utf-8-codec) ; Accounts for s48 bug - new ports are latin-1
	port))

(define linebuf-output-fdport-handler
  (let* ((handler 
			(make-buffered-output-port-handler
				(lambda (cell)
					(list 'linebuf-output-fdport (channel-cell-ref cell)))
				port-channel-closer
				empty-buffer!
				channel-port-ready?))
         (char-handler (port-handler-char handler)))
		; Remaking the port because s48 does not define a setter for handlers :<
		(make-port-handler 
			(port-handler-discloser handler)
			(port-handler-close handler) 
			(port-handler-byte handler)
			; Line buffering only makes sense for write-char
			(lambda (port ch) 
				(char-handler port ch)
				(if (char=? ch #\newline) 
					(force-output port))) ; Raise error if port is not open
			(port-handler-block handler)
			(port-handler-ready? handler)
			(port-handler-force handler))))

(define (make-linebuf-output-fdport channel buffer-size closer)
	(let ((port
	       (make-buffered-output-port 
				linebuf-output-fdport-handler
				(make-channel-cell channel closer)
				(make-byte-vector buffer-size 0)
				0
				buffer-size)))
	; (periodically-force-output! port) ; TODO do we want to enable this at all in scsh? maybe bufpol/auto?
	(add-finalizer! port force-output-if-open) ; s48 forces output if it gc's a an open output port; do we want this?
	(set-port-crlf?! port (channel-crlf?))
	(set-port-text-codec! port utf-8-codec) ; Accounts for s48 bug - new ports are latin-1
	port))

;----------------
; Unbuffered Output ports

(define unbuffered-output-handler
  (make-unbuffered-output-port-handler 
  		(lambda (port)
			(list 'unbuf-output-fdport (channel-cell-ref (port-data port))))
		(lambda (port)
			(port-channel-closer (port-data port)))
		(lambda (port buffer start count)
			(channel-write (channel-cell-ref (port-data port)) buffer start count))
		(lambda (port)			; ready
			(channel-ready? (channel-cell-ref (port-data port))))))

(define (make-unbuf-output-fdport channel closer)
	(let ((port 
			(make-unbuffered-output-port unbuffered-output-handler
				(make-channel-cell channel closer))))
	(set-port-crlf?! port (channel-crlf?))
	(set-port-text-codec! port utf-8-codec) ; Accounts for s48 bug - new ports are latin-1
	port))


; Utilities

(define (channel-buffer-size)
  (channel-parameter (enum channel-parameter-option buffer-size)))

(define (channel-crlf?)
  (channel-parameter (enum channel-parameter-option crlf?)))

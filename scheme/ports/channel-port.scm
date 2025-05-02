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
  (really-make-channel-cell channel closer condvar bufpol soft-bufsize i/o-started? in-use?)
  (in-use? sent)
  channel-cell?
  (channel channel-cell-ref)
  (closer  channel-cell-closer)
  (condvar channel-cell-condvar)
  (bufpol  channel-cell-bufpol  set-channel-cell-bufpol!)
  (soft-bufsize  channel-cell-soft-bufsize  set-channel-cell-soft-bufsize!) ; user specified buffer size
  (i/o-started? channel-cell-i/o-started? set-channel-cell-i/o-started?!)   ; if yes, we cannot change the bufpol
  (in-use? channel-cell-in-use? set-channel-cell-in-use?!)
  (sent    channel-cell-sent    set-channel-cell-sent!))

(define (make-channel-cell channel closer bufpol bufsize)
  (really-make-channel-cell channel closer (make-condvar) bufpol bufsize #f #f))

; TODO: transfer all lookup table stuff here

; Extracting bufpol from fdport
(define (fdport->bufpol port)
  (let ((data (port-data port)))
    (if (channel-cell? data)
		(channel-cell-bufpol data)
		(assertion-violation 'fdport->channel
			"not an fd port" fdport->channel port))))

; Extracting the channel from fdport
(define (fdport->channel port)
  (let ((data (port-data port)))
    (if (channel-cell? data)
		(channel-cell-ref data)
		(assertion-violation 'fdport->channel
			"not an fd port" fdport->channel port))))

; Extracting bufpol from fdport
(define (fdport-i/o-started? port)
  (let ((data (port-data port)))
    (if (channel-cell? data)
		(channel-cell-i/o-started? data)
		(assertion-violation 'fdport-i/o-started?
			"not an fd port" fdport-i/o-started? port))))

; Extracting the fd from fdport
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

(define (reset-fdport-for-bufpol port bufpol bufsize input?)
	(let ((cell (port-data port)))
		(set-channel-cell-bufpol! cell bufpol)
		(set-channel-cell-soft-bufsize! cell bufsize)
		(set-port-index! port 0)
		(set-port-limit! port (if input? 0 (channel-buffer-size)))))

;----------
; Input ports

; An unbuffered read reads (or attempts to read) excatly the number of bytes
; requested in a single syscall. This behavior is straighforward for reading byte
; streams, but it gets significantly more involved if we want to read variable-length
; char encodings one character at a time. To approximate the behavior, we will read one 
; byte at a time until we either can decode a character OR we have exhausted max char length 
; for a given encoding.  

; Buffer filler for input ports has four states:
;   A. there is no read in progress
;       -> initiate a read
;   B. a read has completed
;       -> update the port
;   C. a read has been started and has not completed
;       -> wait for it
;   D. we don't want to wait
;       -> so we don't

(define (bufpol-buffer-filler port wait?) 
  (let ((cell (port-data port))
		(buffer (port-buffer port)))
	(set-channel-cell-i/o-started?! cell #t)
    (let ((unbuf? (buf-policy=? (channel-cell-bufpol cell) bufpol/none))
		  (soft-bufsize (channel-cell-soft-bufsize cell))
		  (condvar (channel-cell-condvar cell))
	  	  (channel (channel-cell-ref cell)))
      (cond 
		((not (channel-cell-in-use? cell))
			(set-channel-cell-in-use?! cell #t)
			(let ((limit (provisional-port-limit port)))
				(channel-maybe-commit-and-read channel
								buffer
								limit
								(if unbuf? 1 (- soft-bufsize limit))
								condvar
								wait?))
			(debug-message "READ")  ; debugging
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

;;; Needed altering for soft bufsize check
; The MODE argument says whether we're doing a READ (#f) , a PEEK (#t),
; or a CHAR-READY? ( () )
(define cr (ascii->char 13))
(define (make-one-char-input buffer-filler!)
  (lambda (port mode)
    (let ((decode
	   (text-codec-decode-char-proc (port-text-codec port))))
      (with-new-proposal (lose)

	(let ((limit (provisional-port-limit port)))
	  (let loop ((index (provisional-port-index port)))
	  
	    (define (consume&deliver decode-count val)
	      (if (not mode)
		  (provisional-set-port-index! port
					       (+ index decode-count)))
	      (if (maybe-commit)
		  val
		  (lose)))

	    (cond ((not (open-input-port? port))
		   (remove-current-proposal!)
		   (assertion-violation (cond
					 ((not mode) 'read-char)
					 ((null? mode) 'char-ready?)
					 (else 'peek-char))
					"invalid argument"
					port))
		  ((< index limit)
		   (let ((buffer (port-buffer port)))
		     (call-with-values
			 (lambda ()
			   (decode buffer index (- limit index)))
		       (lambda (ch decode-count)
			 (cond
			  (ch
			    ;; CR/LF handling. Great.
			   (cond
			    ((port-crlf? port)
			     (cond
			      ((char=? ch cr)
			       (provisional-set-port-pending-cr?! port #t)
			       (consume&deliver decode-count
						(if (null? mode) ; CHAR-READY?
						    #t
						    #\newline)))
			      ((and (char=? ch #\newline)
				    (provisional-port-pending-cr? port))
			       (provisional-set-port-pending-cr?! port #f)
			       (loop (+ index decode-count)))
			      (else
			       (provisional-set-port-pending-cr?! port #f)
			       (consume&deliver decode-count
						(if (null? mode) ; CHAR-READY?
						    #t
						    ch)))))
			    (else
			     (provisional-set-port-pending-cr?! port #f)
			     (consume&deliver decode-count
					      (if (null? mode) ; CHAR-READY?
						  #t
						  ch)))))
			     
			  ((or (not decode-count) ; decoding error
			       (provisional-port-pending-eof? port)) ; partial char
			   (consume&deliver 1
					    (if (null? mode)
						#t
						#\?)))
			  ;; need at least DECODE-COUNT bytes
			  (else
			   (if (> decode-count
				  (- (channel-cell-soft-bufsize (port-data port))
				     limit))
			      
			       ;; copy what we have to the
			       ;; beginning so there's space at the
			       ;; end we can try to fill
			       (begin
				 ;; (debug-message "aligning port buffer")
				 (attempt-copy-bytes! buffer index
						      buffer 0
						      (- limit index))
				 (provisional-set-port-index! port 0)
				 (provisional-set-port-limit! port (- limit index))))
			   (if (or (not (buffer-filler! port (not (null? mode))))
				   (not (null? mode)))
			       (lose)
			       #f)))))))
		  ((provisional-port-pending-eof? port)
		   (if (not mode)
		       (provisional-set-port-pending-eof?! port #f))
		   (cond
		    ((not (maybe-commit))
		     (lose))
		    ((null? mode) #t)
		    (else (eof-object))))
		  (else
		   (if (= index limit)	; we have zilch
		       (begin
			 (provisional-set-port-index! port 0)
			 (provisional-set-port-limit! port 0))
		       ;; may be out of synch because of CR/LF conversion
		       (provisional-set-port-index! port index))
		   (if (or (not (buffer-filler! port (not (null? mode))))
			   (not (null? mode)))
		       (lose)
		       #f)))))))))
			


(define bufpol-input-fdport-handler
  (let ((handler 
			(make-buffered-input-port-handler
				(lambda (cell)
				(list 'input-fdport
						'bufpol: (channel-cell-bufpol cell) 
						'bufsize: (channel-cell-soft-bufsize cell) 
						'os-path: (channel-id (channel-cell-ref cell))))
				port-channel-closer
				bufpol-buffer-filler
				channel-port-ready?)))
		; Remaking the port because s48 does not define a setter for char handler :<
		(make-port-handler 
			(port-handler-discloser handler)
			(port-handler-close handler) 
			(port-handler-byte handler)
			; A bit chaned char handler that is soft-bufsize aware
			(make-one-char-input bufpol-buffer-filler)
			(port-handler-block handler) 
			(port-handler-ready? handler)
			(port-handler-force handler))))

; Assuming all arguments are valid 
(define unbuf-port-buf-size 64) ; Arbitrary small vlaue for unbuffered buffers
(define (really-make-input-fdport channel bufpol buffer-size closer)
	(let* ((bufsize (if (buf-policy=? bufpol bufpol/none)
						unbuf-port-buf-size
						buffer-size))
		   (port
			(make-buffered-input-port 
				bufpol-input-fdport-handler
				(make-channel-cell channel closer bufpol bufsize)
				(make-byte-vector (channel-buffer-size)  0)
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
    (channel-maybe-commit-and-write 
		(channel-cell-ref cell)
		(port-buffer port)
		sent
		(- (provisional-port-index port) sent)
		(channel-cell-condvar cell)
		wait?)))


(define (make-one-byte-output buffer-emptier!)
  (lambda (port byte)
    (with-new-proposal (lose)
      (let ((index (provisional-port-index port))
	  		(limit (channel-cell-soft-bufsize (port-data port))))
	(cond ((not (open-output-port? port))
	       (remove-current-proposal!)
	       (assertion-violation 'write-byte "invalid argument" port))
	      ((< index limit)
	       (provisional-byte-vector-set! (port-buffer port)
					     index
					     byte)
	       (provisional-set-port-index! port (+ 1 index))
	       (or (maybe-commit)
		   (lose)))
	      (else
	       (call-to-flush! port (lambda () (buffer-emptier! port #t)))
	       (lose)))))))

(define (make-one-char-output buffer-emptier!)
  (lambda (port ch)
    (let ((encode
	   (text-codec-encode-char-proc (port-text-codec port))))
      (with-new-proposal (lose)
	(let ((index (provisional-port-index port))
	      (limit (channel-cell-soft-bufsize (port-data port))))
	  (cond ((not (open-output-port? port))
		 (remove-current-proposal!)
		 (assertion-violation 'write-byte "invalid argument" port))
		((< index limit)
		 (let ((encode-count #f)
		       (ok? #f))
		   (cond
		    ((not
		      (maybe-commit-no-interrupts
		       (lambda ()
			 (if (and (port-crlf? port)
				  (char=? ch #\newline))
			     ;; CR/LF handling ruins our day once again
			     (call-with-values
				 (lambda ()
				   (encode cr
					   (port-buffer port)
					   index (- limit index)))
			       (lambda (the-ok? cr-encode-count)
				 (cond
				  ((or (not the-ok?)
				       (>= (+ index cr-encode-count) limit))
				   (set! ok? #f)
			   (set! encode-count (+ 1 cr-encode-count))) ; LF will take at least one
				  (else
				   (call-with-values
				       (lambda ()
					 (encode #\newline
						 (port-buffer port)
						 (+ index cr-encode-count)
						 (- limit (+ index cr-encode-count))))
				     (lambda (the-ok? lf-encode-count)
				       (set! ok? the-ok?)
				       (if the-ok?
					   (set-port-index! port
							    (+ index
							       cr-encode-count lf-encode-count))
					   (set! encode-count (+ cr-encode-count lf-encode-count)))))))))
			     (call-with-values
				 (lambda ()
				   (encode ch
					   (port-buffer port)
					   index (- limit index)))
			       (lambda (the-ok? the-encode-count)
				 (set! ok? the-ok?)
				 (if the-ok?
				     (set-port-index! port (+ index the-encode-count))
				     (set! encode-count the-encode-count))))))))
		     (lose))
		    (ok?)		; we're done
		    (encode-count	; need more space
		     (with-new-proposal (_)
		       (call-to-flush! port (lambda () (buffer-emptier! port #t))))
		     (lose))
		    (else		; encoding error
		     (set! ch #\?)    ; if we get an encoding error on
					; the second go, we're toast
		     (lose)))))
		(else
		 (call-to-flush! port (lambda () (buffer-emptier! port #t)))
		 (lose))))))))

; We have the following possibilities:
;  - the port is no longer open
;       -> raise an error
;  - there is nothing to write
;       -> do nothing
;  - there is room left in the port's buffer
;       -> copy bytes into it
;  - there is no room left in the port's buffer
;       -> write it out and try again

(define (make-write-block buffer-emptier!)
  (lambda (port buffer start count)
    (let loop ((sent 0))
      (with-new-proposal (lose)
	(cond ((not (open-output-port? port))
	       (remove-current-proposal!)
	       (assertion-violation 'write-block "invalid argument"
				    buffer start count port))
	      ((= count 0)
	       (if (maybe-commit)
		   0
		   (lose)))
	      ((copy-bytes-out! buffer
				(+ start sent)
				(- count sent)
				port)
	       => (lambda (more)
		    (if (maybe-commit)
			(let ((sent (+ sent more)))
			  (if (< sent count)
			      (loop sent)))
			(lose))))
	      (else
	       (call-to-flush! port (lambda () (buffer-emptier! port #t)))
	       (lose)))))))

(define (copy-bytes-out! buffer start count port)
  (let ((index (provisional-port-index port))
  		(limit (channel-cell-soft-bufsize (port-data port))))
    (if (< index limit)
	(let ((copy-count (min (- limit index)
			       count)))
	  (check-buffer-timestamp! port)	; makes the proposal check this
	  (provisional-set-port-index! port (+ index copy-count))
	  (attempt-copy-bytes! buffer start
			       (port-buffer port) index
			       copy-count)
	  copy-count)
	#f)))


;

(define bufpol-output-fdport-handler
  (let ((buf-handler ; use for discloser, closer, ready-char and forcer
			(make-buffered-output-port-handler
				(lambda (cell)
				  (list 'output-fdport
						'bufpol:  (channel-cell-bufpol cell) 
						'bufsize: (channel-cell-soft-bufsize cell) 
						'os-path: (channel-id (channel-cell-ref cell))))
				port-channel-closer
				empty-buffer!
				channel-port-ready?))
		(unbuf-handler ; use for write handlers for unbuffered bufpol
			(make-unbuffered-output-port-handler 
				(lambda (port) (list 'dummy))     ; discloser
				(lambda (port) (list 'dummy))     ; closer
				(lambda (port buffer start count) ; write-block
					(set-channel-cell-i/o-started?! (port-data port) #t)
					(channel-write (channel-cell-ref (port-data port)) buffer start count))
				(lambda (port)			          ; ready
					(channel-ready? (channel-cell-ref (port-data port))))))
		; remade buffered writehandlers that are aware of soft bufsize 
		(buf-byte-handler (make-one-byte-output empty-buffer!))
		(buf-char-handler (make-one-char-output empty-buffer!))
		(buf-block-handler (make-write-block empty-buffer!)))
		; Remaking the port because s48 does not define a setter for char handler :<
		(make-port-handler 
			(port-handler-discloser buf-handler)
			(port-handler-close buf-handler)
			;;;;
			; byte handler
			(lambda (port byte) 
			 (let* ((cell (port-data port))
				    (bufpol (channel-cell-bufpol cell)))
			   (set-channel-cell-i/o-started?! cell #t)
			   (cond 
				((buf-policy=? bufpol bufpol/none) ((port-handler-byte unbuf-handler) port byte))
				(else (buf-byte-handler port byte))))) ; line and block
			; char handler
			(lambda (port char) 
			 (let* ((cell (port-data port))
				    (bufpol (channel-cell-bufpol cell)))
			   (set-channel-cell-i/o-started?! cell #t)
			   (cond 
			    ((buf-policy=? bufpol bufpol/block) (buf-char-handler port char))
				((buf-policy=? bufpol bufpol/line) 
					(buf-char-handler port char)
					(if (char=? char #\newline) (force-output port)))
				((buf-policy=? bufpol bufpol/none) ((port-handler-char unbuf-handler) port char)))))
			; block handler
			(lambda (port buffer start count)
			 (let* ((cell (port-data port))
				    (bufpol (channel-cell-bufpol cell)))
			   (set-channel-cell-i/o-started?! cell #t)
			   (cond 
			   	((buf-policy=? bufpol bufpol/none) ((port-handler-block unbuf-handler) port buffer start count))
				(else (buf-block-handler port buffer start count))))) ; line and block
			;;;;
			(port-handler-ready? buf-handler)
			(port-handler-force buf-handler))))

(define (really-make-output-fdport channel bufpol buffer-size closer)
	(let* ((bufsize (if (buf-policy=? bufpol bufpol/none)
						unbuf-port-buf-size ; not really important because unbuf will mostly bypass buffer, leave for consistency
						buffer-size))
		   (port
			(make-buffered-output-port 
				bufpol-output-fdport-handler
				(make-channel-cell channel closer bufpol bufsize)
				(make-byte-vector (channel-buffer-size) 0)
				0
				(channel-buffer-size)))) ; port limit that we don't actually care about
	; (periodically-force-output! port) ; TODO do we want to enable this at all in scsh? maybe bufpol/auto?
	(register-flushable-port! port)
	(add-finalizer! port force-output-if-open) ; s48 forces output if it gc's a an open output port; do we want this?
	(set-port-crlf?! port (channel-crlf?))
	(set-port-text-codec! port utf-8-codec) ; Accounts for s48 bug - new ports are latin-1
	port))

;----------------
; Code to 

; And a current field.

(define port-flushed port-pending-eof?)
(define set-port-flushed! set-port-pending-eof?!)

(define flushable-ports
  (make-session-data-slot! (list #f)))

(define (register-flushable-port! port)
  (let ((pair (session-data-ref flushable-ports)))
    (set-cdr! pair
	      (cons (make-weak-pointer port)
		    (cdr pair)))))

; Return a list of thunks that will flush the buffer of each open port
; that contains bytes that have been there since the last time
; this was called.  The actual i/o is done using separate threads to
; keep i/o errors from killing anything vital.
; 
; If USE-FLUSHED-FLAGS? is true this won't flush buffers that have been
; flushed by someone else since the last call.  If it is false then flush
; all non-empty buffers, because the system has nothing to do and is going
; to pause while waiting for external events.

(define (output-fdport-forcers use-flushed-flags?)
  (let ((pair (session-data-ref flushable-ports)))
    (let loop ((next (cdr pair))
	       (last pair)
	       (thunks '()))
      (if (null? next)
;	  (begin   (debug-message "[forcing "
;				  (length thunks)
;				  " thunk(s)]")
	  thunks ;)
	  (let ((port (weak-pointer-ref (car next))))
	    (cond ((or (not port)	; GCed or closed so
		       (not (open-output-port? port))) ; drop it from the list
		   (set-cdr! last (cdr next))
		   (loop (cdr next) last thunks))
		  ((eq? (port-flushed port) 'flushing) ; somebody else is doing it
		   (loop (cdr next) next thunks)) 
		  ((and use-flushed-flags? ; flushed recently
			(port-flushed port))
		   (set-port-flushed! port #f)	; race condition, but harmless
		   (loop (cdr next) next thunks))
		  ((< 0 (port-index port)) ; non-empty
		   (loop (cdr next) next
			 (cons (make-forcing-thunk port)
			       thunks)))
		  (else			; empty
		   (loop (cdr next) next thunks))))))))

(define (make-forcing-thunk port)
  (lambda ()
;    (debug-message "[forcing port]")
    (if (and (report-errors-as-warnings
	       (lambda ()
		 (force-output-if-open port))
	       "error when flushing buffer; closing port"
	       port)
	     (open-output-port? port))
	(report-errors-as-warnings
	  (lambda ()
	    (atomically! (set-port-index! port 0))	; prevent flushing
	    ((port-handler-close (port-handler port))
	     port))
	  "error when closing port"
	  port))))

(define (call-to-flush! port thunk)
  (set-port-flushed! port 'flushing) ; don't let the periodic flusher go crazy
  (thunk)
  (set-port-flushed! port #t))
  
;;
; Utilities

(define (channel-buffer-size)
  (channel-parameter (enum channel-parameter-option buffer-size)))

(define max-soft-bufsize (channel-buffer-size))

(define (channel-crlf?)
  (channel-parameter (enum channel-parameter-option crlf?)))

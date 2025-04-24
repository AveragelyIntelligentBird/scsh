;; Enum, represeting possile buffering policies
(define-enumerated-type buf-policy :buf-policy
  buf-policy?				; predicate
  buf-policies          ; vector containing all elements
  buf-policy-name			; name accessor
  buf-policy-index			; index accessor
  ( 
    block   ; General block buffering (general default)
    line    ; Line buffering (tty default) 
    none    ; Direct I/O -- no buffering
  ))

(define bufpol/block (buf-policy block))
(define bufpol/line (buf-policy line))
(define bufpol/none (buf-policy none))

(define (buf-policy=? p1 p2)
  (= (buf-policy-index p1) (buf-policy-index p2)))

;; Updating port handlers to respect buffering policies

(define (dummy-port-with-buf port)
  (let ((handler (port-handler port)))
  (make-port 
    (make-port-handler 
      (port-handler-discloser handler)
      (lambda (port) (if #f #f)) ; No closer
      (port-handler-byte handler)
      (port-handler-char handler)
      (port-handler-block handler)
      (port-handler-ready? handler)
      (port-handler-force handler))
    (port-text-codec-spec port) ; Preserve a set codec
    (port-crlf? port)
    (port-status port)
    (port-lock port)
    (port-data port)            ; Preserve channel reference
    (make-byte-vector 128 0)    ; Same as unbuf outport 
    (port-index port)
    (port-limit port)
    (port-pending-cr? port)
    (port-pending-eof? port))))

(define (remake-handler/in-unbuf port handler)
  (let ((char-handler (port-handler-char handler))
        (dummy-buf-port (dummy-port-with-buf port)))
    (make-port-handler 
      (port-handler-discloser handler)
      (port-handler-close handler)
      (port-handler-byte handler)
      (lambda (port mode) 
        (char-handler dummy-buf-port mode))
      (port-handler-block handler)
      (port-handler-ready? handler)
      (port-handler-force handler))))

(define (remake-port/handler port fresh-port handler)
  (make-port handler
    (port-text-codec-spec port) ; Preserve a set codec
    (port-crlf? fresh-port)
    (port-status fresh-port)
    (port-lock fresh-port)
    (port-data port)            ; Preserve channel reference
    (port-buffer fresh-port)
    (port-index fresh-port)
    (port-limit fresh-port)
    (port-pending-cr? fresh-port)
    (port-pending-eof? fresh-port)))

(define (remake-handler/out-linebuf handler)
  (let ((forcer (port-handler-force handler))
        (char-handler (port-handler-char handler)))
    (make-port-handler 
      (port-handler-discloser handler)
      (port-handler-close handler)
      (port-handler-byte handler)
      (lambda (port ch) 
        (char-handler port ch)
        (if (char=? ch #\newline) 
            (forcer port #t))) ; Raise error if port is not open
      (port-handler-block handler)
      (port-handler-ready? handler)
      forcer)))

(define (make-fresh-inport-from port size)
  (let ((os-channel (port->channel port)))
  (cond 
    ((not size)   ; If size not given, use s48 default 
      (input-channel+closer->port os-channel close-fdport-channel))
    ((>= size 1)   ; If greater than 1, pass it along (size=1  is unbuf)
      (input-channel+closer->port os-channel close-fdport-channel size))
    (else 
      (error "For inport block buffering, buffer size must be a positive int")))))

(define (make-fresh-outport-from port size)
  (let ((os-channel (port->channel port)))
  (cond 
    ((not size)  ; If size not given, use s48 default 
      (output-channel+closer->port os-channel close-fdport-channel))
    ((> size 0)  ; If positive, pass it along
      (output-channel+closer->port os-channel close-fdport-channel size))
    ((zero? size) ; If zero, this creates an unbuffered outport
      ; We don't care about closer here because handler derives closer from data, which we will update
      (output-channel->port os-channel size)) 
    (else 
      (error "For outport block buffering, buffer size must be a non-negative int")))))

(define (remake-port/bufpol port policy size)
  (cond
    ; Input port
    ((open-input-port? port)
    (let* ((policy (if (and size (= 1 size)) bufpol/none policy))
           (size (if (and (not size) (buf-policy=? policy bufpol/none)) 1 size))
           (fresh-inport (make-fresh-inport-from port size))
           (fresh-inport-handler (port-handler fresh-inport))) 
      (cond 
        ((buf-policy=? policy bufpol/block)
          ; Regular buffered inport
          (remake-port/handler 
            port                    ; take codec, data
            fresh-inport           ; take everything else
            fresh-inport-handler)) ; use this handler
        ((buf-policy=? policy bufpol/line)
          ; Error, doesn't make sense
          (error "Cannot set line buffering policy on an input port"))
        ((buf-policy=? policy bufpol/none)
          ; Unbuffered inport with custom buffer-filler
          (remake-port/handler 
            port                    ; take codec, data
            fresh-inport           ; take everything else
            ; Replaces char with a dummy port clousre
            (remake-handler/in-unbuf port fresh-inport-handler))) 
        (else (error "Invalid buffering policy")))))
    ; Output ports
    ((open-output-port? port)
     (flush-fdport port) ;; flush buffer before changing policy; potentially TODO better handling
     (let* ((policy (if (and size (zero? size)) bufpol/none policy))
            (size (if (and (not size) (buf-policy=? policy bufpol/none)) 0 size))
            (fresh-outport (make-fresh-outport-from port size))
            (fresh-outport-handler (port-handler fresh-outport))) 
      (cond 
        ((buf-policy=? policy bufpol/block)
          ; Regular buffered outport
          (remake-port/handler 
            port                    ; take codec, data
            fresh-outport           ; take everything else
            fresh-outport-handler)) ; use this handler
        ((buf-policy=? policy bufpol/line)
          (remake-port/handler 
            port                   ; take codec, data
            fresh-outport          ; take everything else
            ; Remake the handler to flush on newline in char handler
            (remake-handler/out-linebuf fresh-outport-handler)))
        ((and (buf-policy=? policy bufpol/none) (not (zero? size)))
          (error "For unbuffered outports, buffer size must be zero"))
        ((buf-policy=? policy bufpol/none)
          ; Regular unbuffered outport
          (remake-port/handler 
            port
            fresh-outport 
            fresh-outport-handler)))))
    (else 
      (error "Cannot set buffering policy on a closed port"))))

(define (set-port-buffering port policy . size)
  (check-arg port? port set-port-buffering)
  (check-arg buf-policy? policy set-port-buffering)
  (let* ((size (if (null? size) #f (car size)))
         (new-port (remake-port/bufpol port policy size))
         (cur-port-fd (port->fd port))
         (cur-port-revealed-count (maybe-ref-fdport-revealed cur-port-fd)))
    (set-fdport! cur-port-fd new-port cur-port-revealed-count)))

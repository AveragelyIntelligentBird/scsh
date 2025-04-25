;;; Scsh portmakers. Construct scsh ports for a given bufpolicy
; We still use s48 port record because it is integrated in the s48 VM as a stob
; Any extensions, such as port reveal count and bufpol, are managed by scsh's lookup
; table, as can be seen in newports.scm

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

;; Actual fdport makers

(define (make-input-fdport channel bufpol . maybe-buffer-size)
  (let ((buffer-size (if (null? maybe-buffer-size) 
                         (channel-buffer-size) 
                         (car maybe-buffer-size))))
  (really-make-input-fdport channel bufpol buffer-size))
)

(define (really-make-input-fdport channel bufpol buffer-size)
    (let* ((bufpol (if (= 1 buffer-size) bufpol/none bufpol))) 
    (cond 
        ; Errors
        ((>= 0 buffer-size) 
            (assertion-violation 'really-make-input-fdport
              "invalid buffer size"
              really-make-input-fdport channel buffer-size))
        ((buf-policy=? bufpol bufpol/line)
          (assertion-violation 'really-make-input-fdport
            "line buffering is invalid on input ports"
            really-make-input-fdport channel bufpol))
        ; Valid ports
        ((buf-policy=? bufpol bufpol/block) ; TODO: do we want to warn about too small buf for block?
          (debug-message "Making block buf port")
          (make-buf-input-fdport channel buffer-size close-fdport-channel))
        ((buf-policy=? bufpol bufpol/none)
          (debug-message "Making unbuf port")
          (make-unbuf-input-fdport channel close-fdport-channel)))))

          
; TODO - implement
(define (really-make-output-fdport channel bufpol . maybe-buffer-size)
    (display "I'm making output port\n") 
    #f)


; Since a different buffering policy implies a different set of handler, we make a 
; new port from scratch on the same channel + set text codec
(define (set-port-buffering port policy . maybe-buffer-size)
  (check-arg fdport? port set-port-buffering)
  (check-arg buf-policy? policy set-port-buffering)
  ; TODO: address non-empty buffer case. Restrict altogether? warn that will be lost? flush for outport, ignore for inports?
    ; (flush-fdport port)
  (let* ((channel (fdport->channel port))
         (buffer-size (if (null? maybe-buffer-size) 
                          (channel-buffer-size) 
                          (car maybe-buffer-size)))
         (new-port (if (input-port? port)
                        (really-make-input-fdport channel policy buffer-size)
                        (really-make-output-fdport channel policy buffer-size)))
         (cur-port-fd (fdport->fd port))
         (cur-port-revealed-count (maybe-ref-fdport-revealed cur-port-fd)))
    (set-port-text-codec! new-port (port-text-codec port)) 
    (set-fdport! cur-port-fd new-port cur-port-revealed-count)
    (set! port new-port))) 

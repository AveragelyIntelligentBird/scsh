;;; Scsh portmakers. Construct scsh ports for a given bufpolicy
; We still use s48 port record because it is integrated in the s48 VM as a stob
; Any extensions, such as port reveal count and bufpol, are managed by scsh's lookup
; table, as can be seen in newports.scm

;; Actual fdport makers

(define (make-input-fdport channel . maybe-buffer-size)
  (let ((buffer-size (if (null? maybe-buffer-size) 
                         max-soft-bufsize 
                         (car maybe-buffer-size))))
  (really-make-input-fdport channel bufpol/block buffer-size))) ; Block buffering default

(define (really-make-input-fdport channel bufpol buffer-size)
  (let* ((bufpol (if (= 1 buffer-size) bufpol/none bufpol))) 
  (cond 
      ; Errors
      ((or (not (integer? buffer-size)) (>= 0 buffer-size)) 
          (assertion-violation 'really-make-input-fdport
            "invalid buffer size"
            really-make-input-fdport channel buffer-size))
      ((not (channel? channel))
          (assertion-violation 'really-make-input-fdport
            "invalid channel"
            really-make-input-fdport channel buffer-size))
      ((buf-policy=? bufpol bufpol/line)
        (assertion-violation 'really-make-input-fdport
          "line buffering is invalid on input ports"
          really-make-input-fdport channel bufpol))
      ; Valid ports
      ((buf-policy=? bufpol bufpol/block) ; TODO: do we want to warn about too small buf for block?
        (debug-message "Making block buf input port")
        (make-blockbuf-input-fdport channel buffer-size close-fdport-channel))
      ((buf-policy=? bufpol bufpol/none)
        (debug-message "Making unbuf input port")
        (make-unbuf-input-fdport channel close-fdport-channel)))))

(define (make-output-fdport channel . maybe-buffer-size)
  (let ((buffer-size (if (null? maybe-buffer-size) 
                         max-soft-bufsize 
                         (car maybe-buffer-size))))
  (really-make-output-fdport channel bufpol/block buffer-size))) ; Block buffering default

; TODO - remove, this is just for testing
(define (make-output-fdport/bufpol channel bufpol . maybe-buffer-size)
  (let ((buffer-size (if (null? maybe-buffer-size) 
                         max-soft-bufsize 
                         (car maybe-buffer-size))))
  (really-make-output-fdport channel bufpol buffer-size)))
          
(define (really-make-output-fdport channel bufpol buffer-size)
  (let* ((bufpol (if (= 0 buffer-size) bufpol/none bufpol))) 
  (cond 
      ; Errors
      ((or (not (integer? buffer-size)) (> 0 buffer-size))
          (assertion-violation 'really-make-output-fdport
            "invalid buffer size"
            really-make-output-fdport channel buffer-size))
      ((not (channel? channel))
          (assertion-violation 'really-make-output-fdport
            "invalid channel"
            really-make-output-fdport channel buffer-size))
      ; Valid ports
      ((buf-policy=? bufpol bufpol/block) ; TODO: do we want to warn about too small buf for block?
        (debug-message "Making block buf output port")
        (make-blockbuf-output-fdport channel buffer-size close-fdport-channel))
      ((buf-policy=? bufpol bufpol/line)
        (debug-message "Making line buf output port")
        (make-linebuf-output-fdport channel buffer-size close-fdport-channel))
      ((buf-policy=? bufpol bufpol/none)
        (debug-message "Making unbuf output port")
        (make-unbuf-output-fdport channel close-fdport-channel)))))


; Since we cannot set a handler on an existing port, we will set the 
; bufpol data field and have the handlers always check that 
(define (set-port-buffering port bufpol . maybe-buffer-size)
  (check-arg fdport? port set-port-buffering)
  (check-arg buf-policy? bufpol set-port-buffering)
  ; TODO: address non-empty buffer case. Restrict altogether? warn that will be lost? flush for outport, ignore for inports?
  ; (flush-fdport port)
  (let* ((input? (input-port? port))
         (buffer-size (if (null? maybe-buffer-size) 
                          max-soft-bufsize 
                          (car maybe-buffer-size)))
         (bufpol (cond ((and input? (= 1 buffer-size)) bufpol/none)
                       ((and (not input?) (= 0 buffer-size)) bufpol/none)
                       ( else bufpol))))               
    (cond 
      ; Errors
      ((or (not (integer? buffer-size)) 
           (and input? (>= 0 buffer-size))
           (and (not input?) (> 0 buffer-size)))
          (assertion-violation 'set-port-buffering
            "invalid buffer size for the given bufpol"
            set-port-buffering bufpol buffer-size))
      ((and input? (buf-policy=? bufpol bufpol/line))
          (assertion-violation 'set-port-buffering
            "cannot set line buffering on input ports"
            set-port-buffering port bufpol))
      (input? (set-fdport-for-bufpol port bufpol buffer-size))
      (else 
        (assertion-violation 'set-port-buffering "TODO: implement me"))))) 

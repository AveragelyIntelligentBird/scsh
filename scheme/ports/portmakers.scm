;;; Scsh portmakers. Construct scsh ports for a given bufpolicy
; We still use s48 port record because it is integrated in the s48 VM as a stob
; Any extensions, such as port reveal count and bufpol, are managed by scsh's lookup
; table, as can be seen in newports.scm

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

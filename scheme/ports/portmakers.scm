;;; Scsh portmakers. Construct scsh ports for a given bufpolicy
; We still use s48 port record because it is integrated in the s48 VM as a stob
; Any extensions, such as port reveal count and bufpol, are managed by scsh's lookup
; table, as can be seen in newports.scm

(define (really-make-input-fdport channel bufpol . maybe-buffer-size)
    (let* ((buffer-size (if (null? maybe-buffer-size) 
                            (channel-buffer-size) 
                            (car maybe-buffer-size)))
           (bufpol (if (= 1 buffer-size) bufpol/none bufpol))) 
    (cond 
        ; Errors
        ((>= 0 buffer-size) 
            (assertion-violation 'make-input-fdport
              "invalid buffer size"
              really-make-input-fdport channel buffer-size))
        ((buf-policy=? bufpol bufpol/line)
          (assertion-violation 'make-input-fdport
            "line buffering is invalid on input ports"
            really-make-input-fdport channel bufpol))
        ; Valid ports
        ((buf-policy=? bufpol bufpol/block) ; TODO: do we want to warn about too small buf for block?
          (make-buf-input-fdport channel buffer-size close-fdport-channel))
        ((buf-policy=? bufpol bufpol/none)
          (make-unbuf-input-fdport channel close-fdport-channel)))))

          
; TODO - implement
(define (really-make-output-fdport channel bufpol . maybe-buffer-size)
    (display "I'm making output port\n") 
    #f)

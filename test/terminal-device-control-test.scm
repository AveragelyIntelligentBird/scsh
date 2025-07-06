;;; Terminal Device Control Tests ---------------------------------------------
;; Part of scsh 1.0. See file COPYING for notices and license.
;; Tests for scsh's TTY and PTY controlling fascilities. 
;; Extends work done by Christoph Hetz

;; Note that these tests expect stdio to be connected to TTY
;; For example, the tests *will* fail if stdin were connected to a pipe via 
;; a shell script

;;; Test data -----------------------------------------------------------------
;; Defines records & constants to be used throughout the module

(define td-in-falgs 770)
(define td-out-flags 3)
(define td-ctrl-flags 19200)
(define td-local-flasg 1482)
(define td-in-speed 1200)
(define td-out-speed 1200)
(define td-min-char 1)       ; noncanonical
(define td-timeout-read 0)   ; noncanonical  
(define td-tty-info (make-tty-info td-in-falgs 
						td-out-flags
						td-ctrl-flags
						td-local-flasg
						td-in-speed
						td-out-speed
						td-min-char
						td-timeout-read))


;;; Test code -----------------------------------------------------------------
;; All procedures return either #t or #f

;; (make-tty-info if of cf lf ispeed ospeed min time)
(define (make-tty-info-test)
	(and 	(= 	 td-in-falgs 	 (tty-info:input-flags td-tty-info))
	     	(= 	 td-out-flags 	 (tty-info:output-flags td-tty-info))
				(= 	 td-ctrl-flags 	 (tty-info:control-flags td-tty-info))
				(= 	 td-local-flasg  (tty-info:local-flags td-tty-info))
				(equal? td-in-speed 	 (tty-info:input-speed td-tty-info))
				(equal? td-out-speed 	 (tty-info:output-speed td-tty-info))
				(= 	 td-min-char     (tty-info:min td-tty-info))
				(=	 td-timeout-read (tty-info:time td-tty-info))))
(add-test! 'make-tty-info 'terminal-device-control make-tty-info-test)

;; (copy-tty-info tty-info-record)

;; Helper for comparing two tty-info records
(define (tty-info-equal? i1 i2)
	(and 	(equal? (tty-info:control-chars i1) (tty-info:control-chars i2))
		 		(= 	 		(tty-info:input-flags i1)   (tty-info:input-flags i2))
	   	  (= 			(tty-info:output-flags i1)  (tty-info:output-flags i2))
	  	  (=      (tty-info:control-flags i1) (tty-info:control-flags i2))
	     	(=      (tty-info:local-flags i1)   (tty-info:local-flags i2))
	     	(equal? (tty-info:input-speed i1)   (tty-info:input-speed i2))
	     	(equal? (tty-info:output-speed i1)  (tty-info:output-speed i2))
	     	(= 	 		(tty-info:min i1) 			 		(tty-info:min i2))
	     	(= 	 		(tty-info:time i1)   		 		(tty-info:time i2))))

(define (copy-tty-info-test)
	(let* ((ti (tty-info))
	   	   (ti-c (copy-tty-info ti)))
		(and (tty-info? ti)
		 	 (tty-info? ti-c)
			 (tty-info-equal? ti ti-c))))
(add-test! 'copy-tty-info 'terminal-device-control copy-tty-info-test)

;; (tty-info [fd/port/fname]) 
(define (tty-info-record-test)
	(let ((ti (tty-info)))
      (and (string? (tty-info:control-chars ti))
	   (or (integer? (tty-info:input-flags ti))
	       (not (tty-info:input-flags ti)))
	   (or (integer? (tty-info:output-flags ti))
	       (not (tty-info:output-flags ti)))
	   (or (integer? (tty-info:control-flags ti))
	       (not (tty-info:control-flags ti)))
	   (or (integer? (tty-info:local-flags ti))
	       (not (tty-info:local-flags ti)))
	   (or (or (integer? (tty-info:input-speed ti))
                   (memq (tty-info:input-speed ti) '(exta extb)))
	       (not (tty-info:input-speed ti)))
	   (or (or (integer? (tty-info:output-speed ti))
                   (memq (tty-info:output-speed ti) '(exta extb)))
	       (not (tty-info:output-speed ti)))
	   (or (integer? (tty-info:min ti))
	       (not (tty-info:min ti)))
	   (or (integer? (tty-info:time ti))
	       (not(tty-info:time ti))))))
(add-test! 'tty-info-record 'terminal-devive-control tty-info-record-test)

;; (open-pty)
(define (open-pty-test)
  (receive (pty-inport tty-name) (open-pty)
		(let ((tty-in (open-input-file tty-name))
			  (pty-out (dup->outport pty-inport)))
			(set-port-buffering pty-out bufpol/none)
			(write 23 pty-out)
			(newline pty-out)
			(force-output pty-out)
			(let ((reply (read tty-in)))
				(close-output-port pty-out) ;; necessary on some systems for proper exit
				(equal? 23 reply)))))
(add-test! 'open-pty 'terminal-device-control open-pty-test)

;; (fork-pty-session thunk)
(define (fork-pty-session-test)
	(receive (process pty-in pty-out tty-name)
    	(fork-pty-session 
			(lambda () 
				(let ((inp (read)))
					(write (string-append inp inp)))
				(newline)))
		(write "hello" pty-out)
		(newline pty-out)
	  	(force-output pty-out)
		(let ((reply (read pty-in)))
			(close-output-port pty-out) ;; necessary on some systems for proper exit
			(string=? "hellohello" reply))))
(add-test! 'fork-pty-session 'terminal-device-control fork-pty-session-test)

;; (set-tty-info/now info [fd/port/fname])
;; (set-tty-info/drain info [fd/port/fname])
;; (set-tty-info/flush info [fd/port/fname])
(define (set-ti-now-test)
	(receive (process pty-in pty-out tty-name)
    (fork-pty-session 
			(lambda () 
				(let ((inp (read)))
					(write (string-append inp inp)))
				(newline)))
		(let* ((ti (copy-tty-info (tty-info pty-out)))
			   (new-local-flags (bitwise-xor ttyl/echo (tty-info:local-flags ti))))
			(set-tty-info:local-flags ti new-local-flags)
			(set-tty-info/now ti pty-out))
		(write "hello" pty-out)
		(newline pty-out)
	  (force-output pty-out)
		(let ((reply (read pty-in)))
			(close-output-port pty-out) ;; necessary on some systems for proper exit
			(string=? "hellohello" reply))))
(add-test! 'set-ti-now 'terminal-device-control set-ti-now-test)


;; (send-tty-break [fd/port/fname duration]) 

;; (drain-tty [fd/port/fname])
;; put buffer in fd
;; pipe

;; (flush-tty/input [fd/port/fname])
;; (flush-tty/output [fd/port/fname])
;; (flush-tty/both [fd/port/fname])

;; (start-tty-output [fd/port/fname])
;; (stop-tty-output [fd/port/fname])
;; (start-tty-input [fd/port/fname])
;; (stop-tty-input [fd/port/fname])

;; (open-control-tty tty-name [flags])

;; (become-session-leader)

;; (tty-process-group fd/port/fname)
;; (set-tty-process-group fd/port/fname pgrp)

;; Constants

; (add-test! 'tty-info-record-posix-indicies-test 'terminal-device-control
;   (lambda ()
;     (and ttychar/delete-char
; 	 ttychar/delete-line
; 	 ttychar/eof
; 	 ttychar/eol
; 	 ttychar/interrupt
; 	 ttychar/quit
; 	 ttychar/suspend
; 	 ttychar/start
; 	 ttychar/stop)))

; (add-test! 'tty-info-record-posix-input-flags 'terminal-device-control
;   (lambda ()
;     (and ttyin/check-parity
; 	 ttyin/ignore-bad-parity-chars
; 	 ttyin/mark-parity-errors
; 	 ttyin/ignore-break
; 	 ttyin/interrupt-on-break
; 	 ttyin/7bits
; 	 ttyin/cr->nl
; 	 ttyin/ignore-cr
; 	 ttyin/nl->cr
; 	 ttyin/input-flow-ctl
; 	 ttyin/output-flow-ctl)))

; (add-test! 'tty-info-record-posix-output-flags 'terminal-device-control
;   (lambda ()
;     ttyout/enable))

; (add-test! 'tty-info-record-delay-constants-for-output-flags 'terminal-device-control
;   (lambda ()
;     (or (and ttyout/bs-delay
; 	     ttyout/bs-delay0
; 	     ttyout/bs-delay1
; 	     ttyout/cr-delay
; 	     ttyout/cr-delay0
; 	     ttyout/cr-delay1
; 	     ttyout/cr-delay2
; 	     ttyout/cr-delay3
; 	     ttyout/ff-delay
; 	     ttyout/ff-delay0
; 	     ttyout/ff-delay1
; 	     ttyout/tab-delay
; 	     ttyout/tab-delay0
; 	     ttyout/tab-delay1
; 	     ttyout/tab-delay2
; 	     ttyout/tab-delayx
; 	     ttyout/nl-delay
; 	     ttyout/nl-delay0
; 	     ttyout/nl-delay1
; 	     ttyout/vtab-delay
; 	     ttyout/vtab-delay0
; 	     ttyout/vtab-delay1
; 	     ttyout/all-delay)
; 	(not (and ttyout/bs-delay
; 		  ttyout/bs-delay0
; 		  ttyout/bs-delay1
; 		  ttyout/cr-delay
; 		  ttyout/cr-delay0
; 		  ttyout/cr-delay1
; 		  ttyout/cr-delay2
; 		  ttyout/cr-delay3
; 		  ttyout/ff-delay
; 		  ttyout/ff-delay0
; 		  ttyout/ff-delay1
; 		  ttyout/tab-delay
; 		  ttyout/tab-delay0
; 		  ttyout/tab-delay1
; 		  ttyout/tab-delay2
; 		  ttyout/tab-delayx
; 		  ttyout/nl-delay
; 		  ttyout/nl-delay0
; 		  ttyout/nl-delay1
; 		  ttyout/vtab-delay
; 		  ttyout/vtab-delay0
; 		  ttyout/vtab-delay1
; 		  ttyout/all-delay)))))

; (add-test! 'tty-info-record-posix-control-flags 'terminal-device-control
;   (lambda ()
;     (and ttyc/char-size
; 	 ttyc/char-size5
; 	 ttyc/char-size6
; 	 ttyc/char-size7
; 	 ttyc/char-size8
; 	 ttyc/enable-parity
; 	 ttyc/odd-parity
; 	 ttyc/enable-read
; 	 ttyc/hup-on-close
; 	 ttyc/no-modem-sync
; 	 ttyc/2-stop-bits)))

; (add-test! 'tty-info-record-4.3+bsd-control-flags 'terminal-device-control
;   (lambda ()
;     (or (and ttyc/ignore-flags
; 	     ttyc/CTS-output-flow-ctl
; 	     ttyc/RTS-input-flow-ctl
; 	     ttyc/carrier-flow-ctl)
; 	(not (and ttyc/ignore-flags
; 		  ttyc/CTS-output-flow-ctl
; 		  ttyc/RTS-input-flow-ctl
; 		  ttyc/carrier-flow-ctl)))))

; (add-test! 'tty-info-record-posix-local-flags 'terminal-device-control
;   (lambda ()
;     (and ttyl/canonical
; 	 ttyl/echo
; 	 ttyl/echo-delete-line
; 	 ttyl/echo-nl
; 	 ttyl/visual-delete
; 	 ttyl/enable-signals
; 	 ttyl/extended
; 	 ttyl/no-flush-on-interrupt
; 	 ttyl/ttou-signal)))

; (add-test! 'tty-info-record-svr4&4.3+bsd-local-flags 'terminal-device-control
;   (lambda ()
;     (or (and ttyl/echo-ctl
; 	     ttyl/flush-output
; 	     ttyl/hardcopy-delete
; 	     ttyl/reprint-unread-chars
; 	     ttyl/visual-delete-line
; 	     ttyl/alt-delete-word
; 	     ttyl/no-kernel-status
; 	     ttyl/case-map)
; 	(not (and ttyl/echo-ctl
; 		  ttyl/flush-output
; 		  ttyl/hardcopy-delete
; 		  ttyl/reprint-unread-chars
; 		  ttyl/visual-delete-line
; 		  ttyl/alt-delete-word
; 		  ttyl/no-kernel-status
; 		  ttyl/case-map)))))






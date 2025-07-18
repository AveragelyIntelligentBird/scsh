;;; Stdio/stdport sync procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; with-current-foo-port procs
;;; ---------------------------

(define (with-current-input-port* port thunk)
  (call-with-current-input-port port thunk))

(define (with-current-output-port* port thunk)
  (call-with-current-output-port port thunk))

(define (with-current-error-port* port thunk)
  (call-with-current-noise-port port thunk))

(define (with-error-output-port* port thunk)
  (call-with-current-noise-port port thunk))

(define-simple-syntax (with-current-input-port port body ...)
  (with-current-input-port* port (lambda () body ...)))

(define-simple-syntax (with-current-output-port port body ...)
  (with-current-output-port* port (lambda () body ...)))

(define-simple-syntax (with-current-error-port port body ...)
  (with-current-error-port* port (lambda () body ...)))

(define-simple-syntax (with-error-output-port port body ...)
  (with-error-output-port* port (lambda () body ...)))

;; TODO what to do with you??
;; (define (stdio->stdports)
;;   (set-current-input-port!  (fdes->inport 0))
;;   (set-current-output-port! (fdes->outport 1))
;;   (set-current-error-port!   (fdes->outport 2)))

; This is probably no a thing anymore either
; just assume that stdio is stdio unless modified for a thunk with 
; with-current-ports (which we also should expose)
; 

(define (stdports->stdio)
  (dup (current-input-port)  0)
  (dup (current-output-port) 1)
  (dup (current-error-port)  2))

(define (with-stdio-ports* thunk)
  (with-current-input-port (fdes->inport 0)
    (with-current-output-port (fdes->outport 1)
      (with-current-error-port (fdes->outport 2)
        (thunk)))))

(define-simple-syntax (with-stdio-ports body ...)
  (with-stdio-ports* (lambda () body ...)))

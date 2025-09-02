;;; Signal System -------------------------------------------------------------
;; Part of scsh 0.7. See file COPYING for notices and license.
;; Signal handling.

;; Currently very short since most of the interface is provided by Scheme48

(import-lambda-definition-2 signal-pid (pid signal) "scsh_kill")

(define (signal-process proc signal)
  (signal-pid (cond ((proc? proc)    (proc:pid proc))
                    ((integer? proc) proc)
                    (else (error "Illegal proc passed to signal-process" proc)))
              (signal-os-number signal)))

(define (signal-process-group proc-group signal)
  (signal-pid (- (cond ((proc? proc-group)    (proc:pid proc-group))
                       ((integer? proc-group) proc-group)
                       (else (error "Illegal proc passed to signal-process-group"
                                    proc-group))))
              (signal-os-number signal)))

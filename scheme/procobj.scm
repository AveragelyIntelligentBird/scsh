;;; Process objects -----------------------------------------------------------
;; Part of scsh 0.7. See file COPYING for notices and license.
;; This is a GC'd abstraction for Unix process id's. Scheme48's implementation
;; handles most of this spec, so this file mostly provides a wrapper around 
;; process-id. 

;; While it's possible to implement our own version of process objects, we 
;; cannot implement wait variants or reaping policies UNLESS we modify 
;; s48's os-signal-hanlder (scheme/posix/signal.scm in Scheme48 1.9.3). 
;; This is because every time s48 receives a SIGCHILD, it launches its own
;; version wait-any with WNOHANG flag. Rewriting this handler is complicated 
;; because it relies on low-level VM operations to service the interrupt.
;; As it stands now, it's not worth the effort because s48's interface addresses
;; the common case well. In future releases, this can be resolved by tighter
;; integration with s48.

;; An earlier version of this file contained scsh's own implementation of 
;; process object. Developed by Olin Shivers and Roderic Morris.

(define proc?     process-id?)
(define proc=?    process-id=?)
(define proc:pid  process-id->integer)

;; Our own lightweight table to allow for pid lookup
;; Weak table ensures garbage collection
(define *process-objects* (make-weak-table))
(make-reinitializer (lambda ()
                      (set! *process-objects* (make-weak-table))))

(define (new-child-proc pid) 
  (let ((proc (integer->process-id pid)))
    (weak-table-set! *process-objects* pid proc)
    proc))

(define (maybe-pid->proc pid)
  (weak-table-ref *process-objects* pid))

(define (pid->proc pid . maybe-probe?)
  (check-arg integer? pid 'pid->proc)
  (let ((probe? (:optional maybe-probe? #f)))
    (or (maybe-pid->proc pid)
        (case probe?
          ((#f)     (error "Given PID has no corresponding process object" pid))
          ((create) (new-child-proc pid))
          (else     #f)))))

(define (->proc x)
  (cond ((proc? x) x)
        ((integer? x) (pid->proc x))
        (else (error "Cannot coerce argument to process object" x))))

(define (wait maybe-proc)
  (let ((proc (->proc maybe-proc)))
    (wait-for-child-process proc)
    (or (process-id-exit-status        proc)
        (process-id-terminating-signal proc))))

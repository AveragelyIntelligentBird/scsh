;;; Process controls ----------------------------------------------------------
;; Part of scsh 0.7. See file COPYING for notices and license.
;; Execs, exits, forks and forks with pipes for launching new processes

;;; Execs ---------------------------------------------------------------------
;; We rely on s48's exec-with-alias because it handles everything we need + 
;; handles stopping and restarting alarm interrupts seamlessly

;;; A low-level interface to exec() syscall that runs with resources aligned
;; (%exec prog arglist env)
;; * PROG is a string/symbol/number, is stringified
;; * ARGLIST is a list of string/symbol/number, is stringified
;; * ENV is either #f, meaning the current environment, or a string->string
;;       alist.
(define (%exec prog arg-list env)
  (with-resources-aligned
   (list environ-resource cwd-resource umask-resource euid-resource egid-resource)
   (lambda ()
     (let ((prog (stringify prog))
           (arg-list (map stringify arg-list))
           (env (if env (alist->env-list env) env)))
    (exec-with-alias prog #f env arg-list)))))

(define (exec/env prog env . arglist)
  (flush-all-ports-blocking)
  (%exec prog (cons prog arglist) env))

(define (exec prog . arglist)
  (apply exec/env prog #f arglist))

(define (exec-path/env prog env . arglist)
  (flush-all-ports-blocking)
  (if (string-index (stringify prog) #\/)

    ;; Contains a slash -- no path search.
    (%exec prog (cons prog arglist) env)

    ;; Try each directory in PATH-LIST.
    (with-resources-aligned
      (list environ-resource cwd-resource umask-resource euid-resource egid-resource)
      (lambda ()
        (let* ((prog (stringify prog))
               (arglist (map stringify (cons prog arglist)))
               (env (if env (alist->env-list env) env)))
          (for-each 
            (lambda (dir)
              (let ((binary (string-append dir "/" prog)))
                (with-handler (lambda (c m) #f)
                              (lambda () (exec-with-alias binary #f env arglist)))))
            (thread-fluid exec-path-list)))
        (error "No executable found." prog arglist)))
    ))

(define (exec-path prog . arglist)
  (apply exec-path/env prog #f arglist))

;;; Searches the pathlist for an executable prog
;;  Returns string on success, #f on failure
(define (exec-path-search prog path-list)
  (let ((prog (stringify prog)))
    (cond ((not (file-name-absolute? prog))
            (let loop ((path-list path-list))
              (if (not (null? path-list))
                  (let* ((dir (car path-list))
                         (fname (string-append dir "/" prog)))
                    (if (file-executable? fname)
                        fname
                        (loop (cdr path-list))))
                  #f)))
          ((file-executable? prog)
            prog)
          (else #f))))

;;; Thread fluid, that captures exec $PATH at startup
(define exec-path-list)
(define (init-exec-path-list)
  (set! exec-path-list
        (make-preserved-thread-fluid
         (cond ((getenv "PATH") => split-colon-list)
               (else (warn "Starting up with no path ($PATH).") '())))))

;; We keep SPLIT-COLON-LIST defined
;; internally so the top-level startup code (INIT-SCSH) can use it
;; to split up $PATH without requiring the field-splitter or regexp code.
(define (split-colon-list clist)
  (let ((len (string-length clist)))
    (if (= 0 len) '()                   ; Special case "" -> ().
        ;; Main loop.
        (let split ((i 0))
          (cond ((string-index clist #\: i) =>
                 (lambda (colon)
                   (cons (substring clist i colon)
                         (split (+ colon 1)))))
                (else (list (substring clist i len))))))))

;;; Exits  --------------------------------------------------------------------

(import-lambda-definition-2 exit/status (status) "scsh_exit")

(import-lambda-definition-2 %exit/status (status) "scsh__exit")

(define (%exit . maybe-status)
  (%exit/status (:optional maybe-status 0))
  (error "Yikes! %exit returned."))

(define (exit . maybe-status)
  (let ((status (:optional  maybe-status 0)))
    (if (not (integer? status))
        (error "non-integer argument to exit"))
    (call-exit-hooks-and-run
     (lambda ()
       (exit/status status)
       (display "The evil undead walk the earth." 2)
       (if #t (error "(exit) returned."))))))

;;; Call THUNK, then die.
;;  A clever definition in a clever implementation allows the caller's stack
;;  and dynamic env to be gc'd away, since this procedure never returns.
(define (call-terminally thunk)  ; TODO: consider using ,go ?
  (with-continuation
   null-continuation
   (lambda ()
     (with-handler
      (lambda (c more)
        (display-condition c (current-error-port))
        (exit 1))
      (lambda ()
        (dynamic-wind
            (lambda () (values))
            thunk
            (lambda () (exit 0))))))))

;; from shift-reset.scm:
(define null-continuation #f)

;;; Forks  --------------------------------------------------------------------

(import-lambda-definition-2 %%fork () "scsh_fork")

(define (fork . maybe-thunk)
  (really-fork #t (:optional maybe-thunk #f)))

(define (%fork . maybe-thunk)
  (really-fork #f (:optional maybe-thunk #f)))

(define (really-fork cleanup? thunk/false)
  (if cleanup?
      (flush-all-ports-blocking))
  (let ((proc #f))
    ;; There was an atomicity problem/race condition -- if a child
    ;; process died after it was forked, but before the scsh fork
    ;; procedure could register the child's procobj in the
    ;; pid/procobj table, then when the SIGCHLD signal-handler reaped
    ;; the process, there would be no procobj for it.  We now inhibit
    ;; our interrupts across the %%FORK and NEW-CHILD-PROC
    ;; operations.
    (with-interrupts-inhibited 
      (lambda ()
        ;; with-env-aligned is not neccessary here but it will
        ;; create the environ object in the parent process which
        ;; could reuse it on further forks
        (let ((pid (with-resources-aligned (list environ-resource)
                      %%fork)))
          (if (zero? pid)
              ;; Child
              (begin
                ; Children are non-interactive.
                (if cleanup? (set-batch-mode?! #t)) 
                ; If thunk is given, call-terminally with interrupts *still* inhibited to ensure other
                ; threads don't get in the way
                ; NOTE: For some reason, s48 seems to *not* actually inhibit the delivery of OS signals 
                ; to signal queues when run with with-interrupts-inhibited. This is a good thing for us 
                ; (we can fork off thunks that block on dequeue-signal!), but it's probably a s48 bug.
                (if thunk/false 
                    (call-terminally thunk/false)))
              ;; Parent, using s48's procobj
              (set! proc (integer->process-id pid))))))
    proc))

;;; Like FORK, but the parent and child communicate via a pipe
;; The pipe connects the parent's stdin to the child's stdout. 
;; This function side-effects the parent by changing its stdin. 

(define (fork/pipe . maybe-thunk)
  (really-fork/pipe fork (:optional maybe-thunk #f)))

(define (%fork/pipe . maybe-thunk)
  (really-fork/pipe %fork (:optional maybe-thunk #f)))

;;; Common code for FORK/PIPE and %FORK/PIPE.
(define (really-fork/pipe forker thunk/false)
  (receive (r w) (pipe)
    (let* ((proc (forker #f)))
      (cond (proc                             ; Parent
              (close w)
              (move->fdes r 0))
            (else                             ; Child
              (with-interrupts-inhibited  
                (lambda ()
                  (close r)
                  (move->fdes w 1)
                  (if thunk/false
                      (with-current-output-port
                        w
                        (call-terminally thunk/false)))))))
      proc)))

;;; FORK/PIPE with a connection list.
;; (FORK/PIPE . m-t) = (apply fork/pipe+ '((1 0)) m-t)
(define (fork/pipe+ conns . maybe-thunk)
  (really-fork/pipe+ fork conns (:optional maybe-thunk #f)))

(define (%fork/pipe+ conns . maybe-thunk)
  (really-fork/pipe+ %fork conns (:optional maybe-thunk #f)))

;;; Common code.
(define (really-fork/pipe+ forker conns thunk/false)
  (let* ((pipes (map (lambda (conn) (call-with-values pipe cons))
                      conns))
         (rev-conns (map reverse conns))
         (froms (map (lambda (conn) (reverse (cdr conn)))
                    rev-conns))
         (tos (map car rev-conns)))

      (let ((proc (forker #f)))
        (cond (proc                          ; Parent
               (for-each (lambda (to r/w)
                           (let ((w (cdr r/w))
                                 (r (car r/w)))
                             (close w)
                             (move->fdes r to)))
                         tos pipes))

              (else                         ; Child
                (with-interrupts-inhibited  
                  (lambda ()
                    (for-each (lambda (from r/w)
                                (let ((r (car r/w))
                                      (w (cdr r/w)))
                                  (close r)
                                  (for-each (lambda (fd) (dup w fd)) from)
                                  (close w))) ; Unrevealed ports win.
                              froms pipes)
                    (if thunk/false
                        (call-terminally thunk/false))))))
        proc)))

;;; Miscellaneous process ops -------------------------------------------------
;; Suspend and process-sleep variants

(define (suspend) (signal-process 0 (signal stop)))

(define (process-sleep secs) 
  (process-sleep-until (+ secs (time-seconds (current-time)))))

(define (process-sleep-until when)
  (let* ((when (floor when))    ; Painful to do real->int in Scheme.
         (when (if (exact? when) when (inexact->exact when))))
    (let lp ()
      (or (%sleep-until when) (lp)))))

(import-lambda-definition-2 %sleep-until (secs) "sleep_until")

;;; Old code stash ------------------------------------------------------------
;; These procedures are not described in the manual, but could be added into 
;; the spec with minimal trouble. Requires testing though.

(define (tail-pipe a b)
  (fork/pipe a)
  (call-terminally b))

(define (tail-pipe+ conns a b)
  (fork/pipe+ conns a)
  (call-terminally b))

;;; Lay a pipeline, one process for each thunk. Last thunk is called
;;; in this process. PIPE* never returns.

(define (pipe* . thunks)
  (letrec ((lay-pipe (lambda (thunks)
                       (let ((thunk (car thunks))
                             (thunks (cdr thunks)))
                         (if (pair? thunks)
                             (begin (fork/pipe thunk)
                                    (lay-pipe thunks))
                             (call-terminally thunk)))))) ; Last one.
    (if (pair? thunks)
        (lay-pipe thunks)
        (error "No thunks passed to PIPE*"))))

;;; Splice the processes into the i/o flow upstream from us.
;;; First thunk's process reads from our stdin; last thunk's process'
;;; output becomes our new stdin. Essentially, n-ary fork/pipe.
;;;
;;; This procedure is so trivial it isn't included.
;;; (define (pipe-splice . thunks) (for-each fork/pipe thunks))

;;; The classic T 2.0 primitive.
;;; This definition works for procedures running on top of Unix systems.
(define (halts? proc) #t)
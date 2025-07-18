;;; The scsh argv switch parser.
;;; Copyright (c) 1995 by Olin Shivers. See file COPYING.

;;; Imports:
;;; COMMAND-PROCESSOR: set-batch-mode?! command-loop
;;;   ENSURES-LOADED: really-ensure-loaded
;;;     ENVIRONMENTS: set-interaction-environment! environment-ref
;;;   environment-define!
;;; ERROR-PACKAGE: error
;;;   EVALUATION: eval
;;;   EXTENDED-PORTS: make-string-input-port
;;; INTERFACES: make-simple-interface
;;;     INTERRUPTS: interrupt-before-heap-overflow!
;;;   PACKAGE-COMMANDS-INTERNAL: user-environment config-package
;;;   get-reflective-tower
;;;   PACKAGE-MUTATION: package-open!
;;; PACKAGES: structure-package structure? make-structure
;;;   make-simple-package
;;; RECEIVING: mv return stuff
;;; SCSH-LEVEL-0-INTERNALS: set-command-line-args!
;;; SCSH-VERSION: scsh-version-string
;;;     HANDLE: with-handler

;;; ensure-loaded and load-into now write to noise-port anyway


(define (get-syntactic-tower env)    ;Returns promise of (eval . env)
  (comp-env-macro-eval (if (package? env)
                           (package->environment env)
                           env)))

(define (scsh-get-structure name config-env)
  (let ((thing (environment-ref config-env name)))
    (cond ((structure? thing) thing)
          (else (assertion-violation 'get-structure "not a structure" name thing)))))

(define (load-quietly filename p)
  (if (not (string? filename))
      (error "not a string in load-quietly" filename))
  (silently (lambda () (load-into filename p))))

(define (really-ensure-loaded noise . structs)
  (silently (lambda ()
              (apply ensure-loaded structs))))

(define (load-library-file file lib-dirs script-file package)
  (cond ((file-name-absolute? file)
         (load-quietly file package))

        ;; Search library dirs for FILE.
        ((find-library-file file lib-dirs script-file) =>
         (lambda (file)
           (load-quietly file package)))  ; Load it.

        (else (error "Couldn't find library file" file lib-dirs))))

;;; The switches:
;;;   -o <struct>   Open the structure in current package.
;;;   -n <package>    Create new package, make it current package.
;;;   -m <struct>   <struct>'s package becomes current package.
;;;
;;;   -l  <file>    Load <file> into current package.
;;; -lm <file>    Load <file> into config package.
;;;     -le <file>              Load <file> into exec package.
;;; -ll <file>    As in -lm, but search the library path list.
;;;     -lel <file>             As in -le, but search the library path list.
;;;
;;; +lp <dir>   Add <dir> onto start of library path list.
;;; lp+ <dir>   Add <dir> onto end of library path list.
;;; +lpe <dir>    As in +lp, but expand env vars & ~user.
;;; lpe+ <dir>    As in lp+, but expand env vars & ~user.
;;; +lpsd     Add the script-file's directory to front of path list
;;; lpsd+     Add the script-file's directory to end of path list
;;; -lp-clear   Clear library path list to ().
;;; -lp-default   Reset library path list to system default.
;;;
;;;                             These two require a terminating -s arg:
;;;   -ds     Load terminating script into current package.
;;;   -dm     Load terminating script into config package.
;;;     -de                     Load terminating script into exec package.
;;;
;;;   -e <entry>    Call (<entry>) to start program.
;;;
;;;       Terminating switches:
;;;   -c <exp>    Eval <exp>, then exit.
;;;   -s <script>   Specify <script> to be loaded by a -ds, -dm, or -de.
;;;   --        Interactive scsh.


;;; Return switch list, terminating switch, with arg, top-entry,
;;; and command-line args.
;;; - We first expand out any initial \ <filename> meta-arg.
;;; - A switch-list elt is either "-ds", "-dm", "-de", or a (switch . arg) pair
;;;   for a -o, -n, -m, -l, or -lm switch.
;;; - Terminating switch is one of {s, c, #f} for -s, -c,
;;;   and -- respectively.
;;; - Terminating arg is the <exp> arg to -c, the <script> arg to -s,
;;;   otherwise #f.
;;; - top-entry is the <entry> arg to a -e; #f if none.
;;; - command-line args are what's left over after picking off the scsh
;;;   switches.

(define (parse-scsh-args args)
  (let lp ((args (meta-arg-process-arglist args))
           (switches '()) ; A list of handler thunks
           (top-entry #f) ; -t <entry>
           (need-script? #f)) ; Found a -ds, -dm, or -de?
    (if (pair? args)
        (let ((arg  (car args))
              (args (cdr args)))

          (cond ((string=? arg "-c")
                 (if (or need-script? top-entry (not (pair? args)))
                     (bad-arg)
                     (values (reverse switches) 'c (car args)
                             top-entry (cdr args))))

                ((string=? arg "-s")
                 (if (not (pair? args))
                     (bad-arg "-s switch requires argument")
                     (values (reverse switches) 's (car args)
                             top-entry (cdr args))))

                ((string=? arg "--")
                 (if need-script?
                     (bad-arg "-ds, -dm, or -de switch requires -s <script>")
                     (values (reverse switches) #f #f top-entry args)))

                ((or (string=? arg "-ds")
                     (string=? arg "-dm")
                     (string=? arg "-de")
                     (string=? arg "+lpsd")
                     (string=? arg "lpsd+")
                     (string=? arg "-lp-default")
                     (string=? arg "-lp-clear"))
                 (lp args (cons arg switches) top-entry #t))

                ((or (string=? arg "-l")
                     (string=? arg "-lm")
                     (string=? arg "-le")
                     (string=? arg "-ll")
                     (string=? arg "-lel")
                     (string=? arg "lp+")
                     (string=? arg "+lp")
                     (string=? arg "lpe+")
                     (string=? arg "+lpe"))
                 (if (pair? args)
                     (lp (cdr args)
                         (cons (cons arg (car args)) switches)
                         top-entry
                         need-script?)
                     (bad-arg "Switch requires argument" arg)))

                ((or (string=? arg "-o")
                     (string=? arg "-n")
                     (string=? arg "-m"))
                 (if (pair? args)
                     (let* ((s (car args))
                            (name (if (and (string=? arg "-n")
                                           (string=? s "#f"))
                                      #f ; -n #f  treated specially.
                                      (string->symbol s))))
                       (lp (cdr args)
                           (cons (cons arg name) switches)
                           top-entry
                           need-script?))
                     (bad-arg "Switch requires argument" arg)))

                ((string=? arg "-e")
                 (lp (cdr args)                  switches
                     (string->symbol (car args)) need-script?))

                (else (bad-arg "Unknown switch" arg))))

        (values (reverse switches) #f #f top-entry '()))))

;;; Do each -ds, -dm, -de, -o, -n, -m, -l/lm/ll, +lp/+lpe/lp+/lpe+, or
;;; -lp-clear/lp-default switch, and return the final result package and a
;;; flag saying if the script was loaded by a -ds, -dm, or -de.

(define (do-switches switches script-file commands-env interaction-env config-env)
  (let lp ((switches switches)
           (script-loaded? #f)
           (interaction-env interaction-env))
    (if (pair? switches)
        (let ((switch (car switches))
              (switches (cdr switches)))
          (cond
           ((equal? switch "-ds")
            (load-quietly script-file interaction-env)
            (lp switches #t interaction-env))

           ((equal? switch "-dm")
            (load-quietly script-file config-env)
            (lp switches #t interaction-env))

           ((equal? switch "-de")
            (load-quietly script-file commands-env)
            (lp switches #t interaction-env))

           ((equal? switch "-lp-clear")
            (clear-lib-dirs!)
            (lp switches script-loaded? interaction-env))

           ((equal? switch "-lp-default")
            (reset-lib-dirs!)
            (lp switches script-loaded? interaction-env))

           ((equal? switch "+lpsd")
            (lib-dirs-prepend-script-dir!)
            (lp switches script-loaded? interaction-env))

           ((equal? switch "lpsd+")
            (lib-dirs-append-script-dir!)
            (lp switches script-loaded? interaction-env))

           ((string=? (car switch) "-l")
            (load-quietly (cdr switch) interaction-env)
            (lp switches script-loaded? interaction-env))

           ((string=? (car switch) "-lm")
            (load-quietly (cdr switch) config-env)
            (lp switches script-loaded? interaction-env))

           ((string=? (car switch) "-le")
            (let ((current-package interaction-env))
              (load-quietly (cdr switch) commands-env)
              (set-interaction-environment! current-package)
              (lp switches script-loaded? interaction-env)))

           ((string=? (car switch) "-ll")
            (load-library-file (cdr switch) (lib-dirs) script-file config-env)
            (lp switches script-loaded? interaction-env))

           ((string=? (car switch) "-lel")
            (let ((current-package interaction-env))
              (load-library-file (cdr switch) (lib-dirs) script-file commands-env)
              (set-interaction-environment! current-package)
              (lp switches script-loaded? interaction-env)))

           ((string=? (car switch) "+lp")
            (lib-dirs-prepend! (cdr switch))
            (lp switches script-loaded? interaction-env))

           ((string=? (car switch) "lp+")
            (lib-dirs-append! (cdr switch))
            (lp switches script-loaded? interaction-env))

           ((string=? (car switch) "+lpe")
            (lib-dirs-prepend! (expand-lib-dir (cdr switch)))
            (lp switches script-loaded? interaction-env))

           ((string=? (car switch) "lpe+")
            (lib-dirs-append! (expand-lib-dir (cdr switch)))
            (lp switches script-loaded? interaction-env))

           ((string=? (car switch) "-o")
            (let ((struct-name (cdr switch)))
              (really-ensure-loaded #f (scsh-get-structure struct-name config-env))
              (package-open! interaction-env (lambda () (scsh-get-structure struct-name config-env)))
              (lp switches script-loaded? interaction-env)))

           ((string=? (car switch) "-n")
            (let* ((name (cdr switch))
                   (pack (new-empty-package name interaction-env)))  ; Contains nothing
              (if name         ; & exports nothing.
                  (let* ((iface  (make-simple-interface #f '()))
                         (struct (make-structure pack iface)))
                    (environment-define! config-env name struct)))
              (set-interaction-environment! pack)
              (lp switches script-loaded? pack)))

           ((string=? (car switch) "-m")
            (let ((struct (scsh-get-structure (cdr switch) config-env)))
              (let ((pack (structure-package struct)))
                (set-interaction-environment! pack)
                (really-ensure-loaded #f struct)
                (lp switches script-loaded? pack))))

           (else (error "Impossible error in do-switches. Report to developers."))))
        (values interaction-env script-loaded?))))

(define (new-empty-package name interaction-env)
  (make-simple-package '() #t
    (get-syntactic-tower interaction-env) name))

(define (with-scsh-initialized thunk)
  (init-home-directory
   (cond ((getenv "HOME") => ensure-file-name-is-nondirectory)
         ;; loosing at this point would be really bad, so some
         ;; paranoia comes in order
         (else (call-with-current-continuation
                (lambda (k)
                  (with-handler
                   (lambda (condition more)
                     (warn "Starting up with no home directory ($HOME).")
                     (k "/"))
                   (lambda ()
                     (user-info:home-dir (user-info (user-uid))))))))))
  (init-exec-path-list)
  (thunk))

(define (parse-switches-and-execute all-args context commands-env interaction-env config-env)
  (receive (switches term-switch term-val top-entry args)
           (parse-scsh-args (cdr all-args))
    (with-handler
      (lambda (cond more)
        (if (error? cond)
            (with-handler
             (lambda (c m)
               (scheme-exit-now 1))
             (lambda ()
               (display-condition cond (current-error-port))
               (scsh-exit-now 1)))
            (more)))
     (lambda ()
       (with-scsh-initialized
         (lambda ()
           ;; Have to do these before calling DO-SWITCHES, because actions
           ;; performed while processing the switches may use these guys.
           (set-command-line-args!
            (cons (if (eq? term-switch 's)
                      term-val  ; Script file.
                      (car all-args)) ;we don't get arg0..
                  args))

           (receive (interaction-env script-loaded?)
                    (do-switches switches term-val commands-env interaction-env config-env)
             (if (not script-loaded?) ; There wasn't a -ds, -dm, or -de,
                 (if (eq? term-switch 's) ; but there is a script,
                     (load-quietly term-val; so load it now.
                                   interaction-env)))

             (cond ((not term-switch) ; -- interactive
                    (scsh-exit-now       ;; TODO: ,exit will bypass this
                     (with-interaction-environment interaction-env
                       (lambda ()
                         (restart-command-processor
                          args
                          context
                          (lambda ()
                            (display (string-append
                                      "Welcome to scsh "
                                      scsh-version-string)
                                     (current-output-port))
                            (newline (current-output-port))
                            (display "Type ,? for help."
                                     (current-output-port))
                            (newline (current-output-port)))
                          values)))))

                   ((eq? term-switch 'c)
                    (let ((result (eval (read-exactly-one-sexp-from-string term-val)
                                        interaction-env)))
                      (scsh-exit-now 0))) 

                   (top-entry   ; There was a -e <entry>.
                    ((eval top-entry interaction-env)
                     (command-line))
                    (scsh-exit-now 0))

                   ;; Otherwise, the script executed as it loaded,
                   ;; so we're done.
                   (else (scsh-exit-now 0))))))))))

(define (read-exactly-one-sexp-from-string s)
  (with-current-input-port (make-string-input-port s)
    (let ((val (read)))
      (if (eof-object? (read)) val
    (error "More than one value read from string" s)))))

(define (scsh-exit-now status)
  (call-exit-hooks-and-run
   (lambda ()
     (scheme-exit-now status))))

(add-exit-hook! flush-all-ports-no-threads)

(define (bad-arg . msg)
  (with-current-output-port (current-error-port)
    (for-each (lambda (x) (display x) (write-char #\space)) msg)
    (newline)
    (display "Usage: scsh [meta-arg] [switch ..] [end-option arg ...]

meta-arg: \\ <script-file-name>

switch: -e <entry-point>  Specify top-level entry point.
  -o <structure>    Open structure in current package.
  -m <package>    Switch to package.
  -n <new-package>  Switch to new package.


  -lm <module-file-name>  Load module into config package.
  -le <exec-file-name>    Load file into exec package.
  -l  <file-name>   Load file into current package.

  -ll <module-file-name>  As in -lm, but search the library path list.
  -lel <exec-file-name>   As in -le, but search the library path list.
  +lp  <dir>    Add <dir> to front of library path list.
  lp+  <dir>    Add <dir> to end of library path list.
  +lpe <dir>    +lp, with env var and ~user expansion.
  lpe+ <dir>    lp+, with env var and ~user expansion.
  +lpsd     Add script-file's dir to front of path list.
  lpsd+     Add script-file's dir to end of path list.
  -lp-clear   Clear library path list to ().
  -lp-default   Reset library path list to system default.

  -ds       Do script.
  -dm     Do script module.
  -de     Do script exec.

end-option: -s <script> Specify script.
    -c <exp>  Evaluate expression.
    --    Interactive session.
" (current-error-port)))
  (exit -1))
